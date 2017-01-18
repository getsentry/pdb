extern crate pdb;
extern crate getopts;

use pdb::FallibleIterator;
use std::env;
use std::fmt;
use std::io::Write;

pub fn type_name<'p>(type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex) -> pdb::Result<String> {
    let mut name = match type_finder.find(type_index)?.parse()? {
        pdb::TypeData::Primitive { primitive_type, indirection } => {
            let mut name = match primitive_type {
                pdb::PrimitiveType::Void => "void".to_string(),
                pdb::PrimitiveType::Char => "char".to_string(),
                pdb::PrimitiveType::UChar => "unsigned char".to_string(),

                pdb::PrimitiveType::I8 => "int8_t".to_string(),
                pdb::PrimitiveType::U8 => "uint8_t".to_string(),
                pdb::PrimitiveType::I16 => "int16_t".to_string(),
                pdb::PrimitiveType::U16 => "uint16_t".to_string(),
                pdb::PrimitiveType::I32 => "int32_t".to_string(),
                pdb::PrimitiveType::U32 => "uint32_t".to_string(),
                pdb::PrimitiveType::I64 => "int64_t".to_string(),
                pdb::PrimitiveType::U64 => "uint64_t".to_string(),

                pdb::PrimitiveType::F32 => "float".to_string(),
                pdb::PrimitiveType::F64 => "double".to_string(),

                pdb::PrimitiveType::Bool8 => "bool".to_string(),

                other => format!("unhandled_primitive_type /* {:?} */", primitive_type),
            };

            match indirection {
                pdb::Indirection::None => {},
                _ => { name.push(' '); name.push('*'); },
            }

            name
        },

        pdb::TypeData::Class { name, .. } => {
            name.to_string().into_owned()
        },

        pdb::TypeData::Enumeration { name, .. } => {
            name.to_string().into_owned()
        },

        pdb::TypeData::Union { name, .. } => {
            name.to_string().into_owned()
        },

        pdb::TypeData::Pointer { underlying_type, .. } => {
            format!("{}*", type_name(type_finder, underlying_type)?)
        },

        pdb::TypeData::Modifier { constant, volatile, underlying_type, .. } => {
            if constant {
                format!("const {}", type_name(type_finder, underlying_type)?)
            } else if volatile {
                format!("volatile {}", type_name(type_finder, underlying_type)?)
            } else {
                // ?
                type_name(type_finder, underlying_type)?
            }
        },

        pdb::TypeData::Array { element_type, dimensions, .. } => {
            let mut name = type_name(type_finder, element_type)?;
            for size in dimensions {
                name = format!("{}[{}]", name, size);
            }
            name
        },

        _ => format!("Type{} /* TODO: figure out how to name it */", type_index)
    };

    if name == "std::basic_string<char,std::char_traits<char>,std::allocator<char> >" {
        name = "std::string".to_string();
    }

    Ok(name)
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct Class<'p> {
    kind: pdb::ClassKind,
    name: pdb::RawString<'p>,
    fields: Vec<Field<'p>>,
}

impl<'p> Class<'p> {
    fn add_derived_from(&mut self, type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex) -> pdb::Result<()> {
        // TODO
        Ok(())
    }

    fn add_fields(&mut self, type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex) -> pdb::Result<()> {
        match type_finder.find(type_index)?.parse()? {
            pdb::TypeData::FieldList { fields, continuation, .. } => {
                for ref field in fields {
                    self.add_field(type_finder, field)?;
                }

                if let Some(continuation) = continuation {
                    // recurse
                    self.add_fields(type_finder, continuation)?;
                }
            }
            other => {
                println!("trying to Class::add_fields() got {} -> {:?}", type_index, other);
                panic!("unexpected type in Class::add_fields()");
            }
        }

        Ok(())
    }

    fn add_field(&mut self, type_finder: &pdb::TypeFinder<'p>, field: &pdb::TypeData<'p>) -> pdb::Result<()> {
        match field {
            &pdb::TypeData::Member { field_type, offset, ref name, .. } => {
                // TODO: attributes (static, virtual, etc.)
                self.fields.push(Field{
                    type_name: type_name(type_finder, field_type)?,
                    name: name.clone(),
                    offset: offset,
                });
            },

            _ => {
                // ignore everything else even though that's sad
            }
        }

        Ok(())
    }
}

impl<'p> fmt::Display for Class<'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} {} {{", match self.kind {
            pdb::ClassKind::Class => "class",
            pdb::ClassKind::Struct => "struct",
            pdb::ClassKind::Interface => "interface",   // when can this happen?
        }, self.name.to_string())?;

        for ref field in &self.fields {
            writeln!(f, "\t/* offset {:3} */ {} {};", field.offset, field.type_name, field.name.to_string())?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct Field<'p> {
    type_name: String,
    name: pdb::RawString<'p>,
    offset: u16,
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct Enum<'p> {
    name: pdb::RawString<'p>,
    values: Vec<EnumValue<'p>>,
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct EnumValue<'p> {
    name: pdb::RawString<'p>,
    value: pdb::EnumValue,
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct Data<'p> {
    classes: Vec<Class<'p>>,
    enums: Vec<Enum<'p>>,
}

impl<'p> fmt::Display for Data<'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "// automatically generated by pdb2hpp\n// do not edit")?;

        for ref class in &self.classes {
            writeln!(f, "")?;
            class.fmt(f)?;
        }

        Ok(())
    }
}

impl<'p> Data<'p> {
    fn new() -> Data<'p> {
        Data{
            classes: Vec::new(),
            enums: Vec::new(),
        }
    }

    fn add(&mut self, type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex) -> pdb::Result<()> {
        match type_finder.find(type_index)?.parse()? {
            pdb::TypeData::Class {
                kind, properties, fields, name, derived_from, ..
            } => {
                // can't do much useful with a forward reference, but also we shouldn't be trying
                // to add them
                assert_eq!(properties.forward_reference(), false);

                // TODO: derived_from

                let mut class = Class{
                    kind: kind,
                    name: name,
                    fields: Vec::new(),
                };

                if let Some(derived_from) = derived_from {
                    class.add_derived_from(type_finder, derived_from)?;
                }

                if let Some(fields) = fields {
                    class.add_fields(type_finder, fields)?;
                }

                self.classes.push(class);
            }

            _ => {
                // ignore
            }
        }

        Ok(())
    }
}

fn write_class(filename: &str, class_name: &str) -> pdb::Result<()> {
    let file = std::fs::File::open(filename)?;
    let mut pdb = pdb::PDB::open(file)?;

    let type_information = pdb.type_information()?;
    let mut type_finder = type_information.new_type_finder();

    let mut data = Data::new();

    let mut type_iter = type_information.iter();
    while let Some(typ) = type_iter.next()? {
        // keep building the index
        type_finder.update(&type_iter);

        if let Ok(pdb::TypeData::Class { name, properties, .. }) = typ.parse() {
            if name.as_bytes() == class_name.as_bytes() && !properties.forward_reference() {
                data.add(&type_finder, typ.type_index())?;
                break;
            }
        }
    }

    if data.classes.len() == 0 {
        writeln!(&mut std::io::stderr(), "sorry, class {} was not found", class_name)
            .expect("stderr write");
    } else {
        println!("{}", data);
    }

    Ok(())
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} input.pdb ClassName", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = getopts::Options::new();
    opts.optflag("h", "help", "print this help menu");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    let (filename, class_name) = if matches.free.len() == 2 {
        (&matches.free[0], &matches.free[1])
    } else {
        print_usage(&program, opts);
        return;
    };

    match write_class(&filename, &class_name) {
        Ok(_) => {}
        Err(e) => {
            writeln!(&mut std::io::stderr(), "error dumping PDB: {}", e)
                .expect("stderr write");
        }
    }
}
