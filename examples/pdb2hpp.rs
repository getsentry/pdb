extern crate pdb;
extern crate getopts;

use pdb::FallibleIterator;
use std::env;
use std::fmt;
use std::io::Write;
use std::collections::BTreeSet;

type TypeSet = BTreeSet<pdb::TypeIndex>;

pub fn type_name<'p>(type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex, mut needed_types: &mut TypeSet) -> pdb::Result<String> {
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

                _ => format!("unhandled_primitive_type /* {:?} */", primitive_type),
            };

            match indirection {
                pdb::Indirection::None => {},
                _ => { name.push(' '); name.push('*'); },
            }

            name
        },

        pdb::TypeData::Class { name, .. } => {
            needed_types.insert(type_index);
            name.to_string().into_owned()
        },

        pdb::TypeData::Enumeration { name, .. } => {
            needed_types.insert(type_index);
            name.to_string().into_owned()
        },

        pdb::TypeData::Union { name, .. } => {
            needed_types.insert(type_index);
            name.to_string().into_owned()
        },

        pdb::TypeData::Pointer { underlying_type, .. } => {
            format!("{}*", type_name(type_finder, underlying_type, needed_types)?)
        },

        pdb::TypeData::Modifier { constant, volatile, underlying_type, .. } => {
            if constant {
                format!("const {}", type_name(type_finder, underlying_type, needed_types)?)
            } else if volatile {
                format!("volatile {}", type_name(type_finder, underlying_type, needed_types)?)
            } else {
                // ?
                type_name(type_finder, underlying_type, needed_types)?
            }
        },

        pdb::TypeData::Array { element_type, dimensions, .. } => {
            let mut name = type_name(type_finder, element_type, needed_types)?;
            for size in dimensions {
                name = format!("{}[{}]", name, size);
            }
            name
        },

        _ => format!("Type{} /* TODO: figure out how to name it */", type_index)
    };

    // TODO: search and replace std:: patterns
    if name == "std::basic_string<char,std::char_traits<char>,std::allocator<char> >" {
        name = "std::string".to_string();
    }

    Ok(name)
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct Class<'p> {
    kind: pdb::ClassKind,
    name: pdb::RawString<'p>,
    base_classes: Vec<BaseClass>,
    fields: Vec<Field<'p>>,
}

impl<'p> Class<'p> {
    fn add_derived_from(&mut self, _: &pdb::TypeFinder<'p>, _: pdb::TypeIndex, _: &mut TypeSet) -> pdb::Result<()> {
        // TODO
        Ok(())
    }

    fn add_fields(&mut self, type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex, mut needed_types: &mut TypeSet) -> pdb::Result<()> {
        match type_finder.find(type_index)?.parse()? {
            pdb::TypeData::FieldList { fields, continuation, .. } => {
                for ref field in fields {
                    self.add_field(type_finder, field, needed_types)?;
                }

                if let Some(continuation) = continuation {
                    // recurse
                    self.add_fields(type_finder, continuation, needed_types)?;
                }
            }
            other => {
                println!("trying to Class::add_fields() got {} -> {:?}", type_index, other);
                panic!("unexpected type in Class::add_fields()");
            }
        }

        Ok(())
    }

    fn add_field(&mut self, type_finder: &pdb::TypeFinder<'p>, field: &pdb::TypeData<'p>, mut needed_types: &mut TypeSet) -> pdb::Result<()> {
        match field {
            &pdb::TypeData::Member { field_type, offset, ref name, .. } => {
                // TODO: attributes (static, virtual, etc.)
                self.fields.push(Field{
                    type_name: type_name(type_finder, field_type, needed_types)?,
                    name: name.clone(),
                    offset: offset,
                });
            },

            &pdb::TypeData::BaseClass { base_class, offset, .. } => {
                self.base_classes.push(BaseClass{
                    type_name: type_name(type_finder, base_class, needed_types)?,
                    offset: offset,
                })
            },

            &pdb::TypeData::VirtualBaseClass { base_class, base_pointer_offset, .. } => {
                self.base_classes.push(BaseClass{
                    type_name: type_name(type_finder, base_class, needed_types)?,
                    offset: base_pointer_offset,
                })
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
        write!(f, "{} {} ", match self.kind {
            pdb::ClassKind::Class => "class",
            pdb::ClassKind::Struct => "struct",
            pdb::ClassKind::Interface => "interface",   // when can this happen?
        }, self.name.to_string())?;

        if self.base_classes.len() > 0 {
            for (i, base) in self.base_classes.iter().enumerate() {
                let prefix = match i {
                    0 => ":",
                    _ => ","
                };
                write!(f, "{} {}", prefix, base.type_name)?;
            }
        }

        writeln!(f, " {{")?;

        for base in &self.base_classes {
            writeln!(f, "\t/* offset {:3} */ /* fields for {} */", base.offset, base.type_name)?;
        }

        for ref field in &self.fields {
            writeln!(f, "\t/* offset {:3} */ {} {};", field.offset, field.type_name, field.name.to_string())?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct BaseClass {
    type_name: String,
    offset: u32,
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
    underlying_type_name: String,
    values: Vec<EnumValue<'p>>,
}

impl<'p> Enum<'p> {
    fn add_fields(&mut self, type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex, mut needed_types: &mut TypeSet) -> pdb::Result<()> {
        match type_finder.find(type_index)?.parse()? {
            pdb::TypeData::FieldList { fields, continuation, .. } => {
                for ref field in fields {
                    self.add_field(type_finder, field, needed_types)?;
                }

                if let Some(continuation) = continuation {
                    // recurse
                    self.add_fields(type_finder, continuation, needed_types)?;
                }
            }
            other => {
                println!("trying to Enum::add_fields() got {} -> {:?}", type_index, other);
                panic!("unexpected type in Enum::add_fields()");
            }
        }

        Ok(())
    }

    fn add_field(&mut self, _: &pdb::TypeFinder<'p>, field: &pdb::TypeData<'p>, _: &mut TypeSet) -> pdb::Result<()> {
        match field {
            &pdb::TypeData::Enumerate { ref name, value, .. } => {
                self.values.push(EnumValue{
                    name: name.clone(),
                    value: value,
                });
            },

            _ => {
                // ignore everything else even though that's sad
            }
        }

        Ok(())
    }
}

impl<'p> fmt::Display for Enum<'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "enum {} /* stored as {} */ {{", self.name.to_string(), self.underlying_type_name)?;

        for value in &self.values {
            writeln!(f, "\t{} = {},", value.name.to_string(), match value.value {
                pdb::EnumValue::U8(v) => format!("0x{:02x}", v),
                pdb::EnumValue::U16(v) => format!("0x{:04x}", v),
                pdb::EnumValue::U32(v) => format!("0x{:08x}", v),
                pdb::EnumValue::U64(v) => format!("0x{:16x}", v),
                pdb::EnumValue::I8(v) => format!("{}", v),
                pdb::EnumValue::I16(v) => format!("{}", v),
                pdb::EnumValue::I32(v) => format!("{}", v),
                pdb::EnumValue::I64(v) => format!("{}", v),
            })?;
        }
        writeln!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct EnumValue<'p> {
    name: pdb::RawString<'p>,
    value: pdb::EnumValue,
}

#[derive(Debug,Clone,PartialEq,Eq)]
struct ForwardReference<'p> {
    kind: pdb::ClassKind,
    name: pdb::RawString<'p>,
}

impl<'p> fmt::Display for ForwardReference<'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} {};", match self.kind {
            pdb::ClassKind::Class => "class",
            pdb::ClassKind::Struct => "struct",
            pdb::ClassKind::Interface => "interface",   // when can this happen?
        }, self.name.to_string())
    }
}


#[derive(Debug,Clone,PartialEq,Eq)]
struct Data<'p> {
    forward_references: Vec<ForwardReference<'p>>,
    classes: Vec<Class<'p>>,
    enums: Vec<Enum<'p>>,
}

impl<'p> fmt::Display for Data<'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "// automatically generated by pdb2hpp\n// do not edit")?;

        if !self.forward_references.is_empty() {
            writeln!(f, "")?;
            for e in self.forward_references.iter() {
                e.fmt(f)?;
            }
        }

        for e in self.enums.iter() {
            writeln!(f, "")?;
            e.fmt(f)?;
        }

        for class in self.classes.iter() {
            writeln!(f, "")?;
            class.fmt(f)?;
        }

        Ok(())
    }
}

impl<'p> Data<'p> {
    fn new() -> Data<'p> {
        Data{
            forward_references: Vec::new(),
            classes: Vec::new(),
            enums: Vec::new(),
        }
    }

    fn add(&mut self, type_finder: &pdb::TypeFinder<'p>, type_index: pdb::TypeIndex, mut needed_types: &mut TypeSet) -> pdb::Result<()> {
        match type_finder.find(type_index)?.parse()? {
            pdb::TypeData::Class {
                kind, properties, fields, name, derived_from, ..
            } => {
                if properties.forward_reference() {
                    self.forward_references.push(ForwardReference{
                        kind: kind,
                        name: name,
                    });

                    return Ok(());
                }

                let mut class = Class{
                    kind: kind,
                    name: name,
                    fields: Vec::new(),
                    base_classes: Vec::new(),
                };

                if let Some(derived_from) = derived_from {
                    class.add_derived_from(type_finder, derived_from, needed_types)?;
                }

                if let Some(fields) = fields {
                    class.add_fields(type_finder, fields, needed_types)?;
                }

                self.classes.insert(0, class);
            }

            pdb::TypeData::Enumeration {
                underlying_type, fields, name, ..
            } => {
                let mut e = Enum{
                    name: name,
                    underlying_type_name: type_name(type_finder, underlying_type, needed_types)?,
                    values: Vec::new(),
                };

                e.add_fields(type_finder, fields, needed_types)?;

                self.enums.insert(0, e);
            }

            other => {
                // ignore
                writeln!(&mut std::io::stderr(), "warning: don't know how to add {:?}", other).expect("stderr write");;
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

    let mut needed_types = TypeSet::new();
    let mut data = Data::new();

    let mut type_iter = type_information.iter();
    while let Some(typ) = type_iter.next()? {
        // keep building the index
        type_finder.update(&type_iter);

        if let Ok(pdb::TypeData::Class { name, properties, .. }) = typ.parse() {
            if name.as_bytes() == class_name.as_bytes() && !properties.forward_reference() {
                data.add(&type_finder, typ.type_index(), &mut needed_types)?;
                break;
            }
        }
    }

    // add all the needed types iteratively until we're done
    loop {
        // get the last element in needed_types without holding an immutable borrow
        let last = match needed_types.iter().next_back() {
            Some(n) => Some(*n),
            None => None
        };

        if let Some(type_index) = last {
            // remove it
            needed_types.remove(&type_index);

            // add the type
            data.add(&type_finder, type_index, &mut needed_types)?;
        } else {
            break
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
