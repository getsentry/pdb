mod types;
mod annotations;
mod constants;

pub use types::*;
pub use self::constants::{CPUType, SourceLanguage};

pub use self::annotations::*;

#[cfg(all(test, feature = "alloc"))]
mod tests {
    mod parsing {
        use crate::{*, symbol::{types::*, *}};

        #[test]
        fn kind_0006() {
            let data = &[6, 0];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x0006);
            assert_eq!(symbol.parse().expect("parse"), SymbolData::ScopeEnd);
        }

        #[test]
        fn kind_1101() {
            let data = &[1, 17, 0, 0, 0, 0, 42, 32, 67, 73, 76, 32, 42, 0];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1101);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::ObjName(ObjNameSymbol {
                    signature: 0,
                    name: "* CIL *".into(),
                })
            );
        }

        #[test]
        fn kind_1102() {
            let data = &[
                2, 17, 0, 0, 0, 0, 108, 22, 0, 0, 0, 0, 0, 0, 140, 11, 0, 0, 1, 0, 9, 0, 3, 91,
                116, 104, 117, 110, 107, 93, 58, 68, 101, 114, 105, 118, 101, 100, 58, 58, 70, 117,
                110, 99, 49, 96, 97, 100, 106, 117, 115, 116, 111, 114, 123, 56, 125, 39, 0, 0, 0,
                0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1102);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Thunk(ThunkSymbol {
                    parent: None,
                    end: SymbolIndex(0x166c),
                    next: None,
                    offset: PdbInternalSectionOffset {
                        section: 0x1,
                        offset: 0xb8c
                    },
                    len: 9,
                    kind: ThunkKind::PCode,
                    name: "[thunk]:Derived::Func1`adjustor{8}'".into()
                })
            );
        }

        #[test]
        fn kind_1105() {
            let data = &[
                5, 17, 224, 95, 151, 0, 1, 0, 0, 100, 97, 118, 49, 100, 95, 119, 95, 97, 118, 103,
                95, 115, 115, 115, 101, 51, 0, 0, 0, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1105);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Label(LabelSymbol {
                    offset: PdbInternalSectionOffset {
                        offset: 0x0097_5fe0,
                        section: 1
                    },
                    flags: ProcedureFlags {
                        nofpo: false,
                        int: false,
                        far: false,
                        never: false,
                        notreached: false,
                        cust_call: false,
                        noinline: false,
                        optdbginfo: false
                    },
                    name: "dav1d_w_avg_ssse3".into(),
                })
            );
        }

        #[test]
        fn kind_1106() {
            let data = &[6, 17, 120, 34, 0, 0, 18, 0, 116, 104, 105, 115, 0, 0];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1106);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::RegisterVariable(RegisterVariableSymbol {
                    type_index: TypeIndex(8824),
                    register: Register(18),
                    name: "this".into(),
                })
            );
        }

        #[test]
        fn kind_110e() {
            let data = &[
                14, 17, 2, 0, 0, 0, 192, 85, 0, 0, 1, 0, 95, 95, 108, 111, 99, 97, 108, 95, 115,
                116, 100, 105, 111, 95, 112, 114, 105, 110, 116, 102, 95, 111, 112, 116, 105, 111,
                110, 115, 0, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x110e);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Public(PublicSymbol {
                    code: false,
                    function: true,
                    managed: false,
                    msil: false,
                    offset: PdbInternalSectionOffset {
                        offset: 21952,
                        section: 1
                    },
                    name: "__local_stdio_printf_options".into(),
                })
            );
        }

        #[test]
        fn kind_1111() {
            let data = &[
                17, 17, 12, 0, 0, 0, 48, 16, 0, 0, 22, 0, 109, 97, 120, 105, 109, 117, 109, 95, 99,
                111, 117, 110, 116, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1111);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::RegisterRelative(RegisterRelativeSymbol {
                    offset: 12,
                    type_index: TypeIndex(0x1030),
                    register: Register(22),
                    name: "maximum_count".into(),
                })
            );
        }

        #[test]
        fn kind_1124() {
            let data = &[36, 17, 115, 116, 100, 0];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1124);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::UsingNamespace(UsingNamespaceSymbol { name: "std".into() })
            );
        }

        #[test]
        fn kind_1125() {
            let data = &[
                37, 17, 0, 0, 0, 0, 108, 0, 0, 0, 1, 0, 66, 97, 122, 58, 58, 102, 95, 112, 117, 98,
                108, 105, 99, 0,
            ];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1125);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::ProcedureReference(ProcedureReferenceSymbol {
                    global: true,
                    sum_name: 0,
                    symbol_index: SymbolIndex(108),
                    module: Some(0),
                    name: Some("Baz::f_public".into()),
                })
            );
        }

        #[test]
        fn kind_1108() {
            let data = &[8, 17, 112, 6, 0, 0, 118, 97, 95, 108, 105, 115, 116, 0];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1108);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::UserDefinedType(UserDefinedTypeSymbol {
                    type_index: TypeIndex(1648),
                    name: "va_list".into(),
                })
            );
        }

        #[test]
        fn kind_1107() {
            let data = &[
                7, 17, 201, 18, 0, 0, 1, 0, 95, 95, 73, 83, 65, 95, 65, 86, 65, 73, 76, 65, 66, 76,
                69, 95, 83, 83, 69, 50, 0, 0,
            ];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1107);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Constant(ConstantSymbol {
                    managed: false,
                    type_index: TypeIndex(4809),
                    value: Variant::U16(1),
                    name: "__ISA_AVAILABLE_SSE2".into(),
                })
            );
        }

        #[test]
        fn kind_110d() {
            let data = &[
                13, 17, 116, 0, 0, 0, 16, 0, 0, 0, 3, 0, 95, 95, 105, 115, 97, 95, 97, 118, 97,
                105, 108, 97, 98, 108, 101, 0, 0, 0,
            ];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x110d);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Data(DataSymbol {
                    global: true,
                    managed: false,
                    type_index: TypeIndex(116),
                    offset: PdbInternalSectionOffset {
                        offset: 16,
                        section: 3
                    },
                    name: "__isa_available".into(),
                })
            );
        }

        #[test]
        fn kind_110c() {
            let data = &[
                12, 17, 32, 0, 0, 0, 240, 36, 1, 0, 2, 0, 36, 120, 100, 97, 116, 97, 115, 121, 109,
                0,
            ];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x110c);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Data(DataSymbol {
                    global: false,
                    managed: false,
                    type_index: TypeIndex(32),
                    offset: PdbInternalSectionOffset {
                        offset: 74992,
                        section: 2
                    },
                    name: "$xdatasym".into(),
                })
            );
        }

        #[test]
        fn kind_1127() {
            let data = &[
                39, 17, 0, 0, 0, 0, 128, 4, 0, 0, 182, 0, 99, 97, 112, 116, 117, 114, 101, 95, 99,
                117, 114, 114, 101, 110, 116, 95, 99, 111, 110, 116, 101, 120, 116, 0, 0, 0,
            ];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1127);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::ProcedureReference(ProcedureReferenceSymbol {
                    global: false,
                    sum_name: 0,
                    symbol_index: SymbolIndex(1152),
                    module: Some(181),
                    name: Some("capture_current_context".into()),
                })
            );
        }

        #[test]
        fn kind_112c() {
            let data = &[44, 17, 0, 0, 5, 0, 5, 0, 0, 0, 32, 124, 0, 0, 2, 0, 2, 0];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };

            assert_eq!(symbol.raw_kind(), 0x112c);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Trampoline(TrampolineSymbol {
                    tramp_type: TrampolineType::Incremental,
                    size: 0x5,
                    thunk: PdbInternalSectionOffset {
                        offset: 0x5,
                        section: 0x2
                    },
                    target: PdbInternalSectionOffset {
                        offset: 0x7c20,
                        section: 0x2
                    },
                })
            );
        }

        #[test]
        fn kind_1110() {
            let data = &[
                16, 17, 0, 0, 0, 0, 48, 2, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 7,
                16, 0, 0, 64, 85, 0, 0, 1, 0, 0, 66, 97, 122, 58, 58, 102, 95, 112, 114, 111, 116,
                101, 99, 116, 101, 100, 0,
            ];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1110);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Procedure(ProcedureSymbol {
                    global: true,
                    dpc: false,
                    parent: None,
                    end: SymbolIndex(560),
                    next: None,
                    len: 6,
                    dbg_start_offset: 5,
                    dbg_end_offset: 5,
                    type_index: TypeIndex(4103),
                    offset: PdbInternalSectionOffset {
                        offset: 21824,
                        section: 1
                    },
                    flags: ProcedureFlags {
                        nofpo: false,
                        int: false,
                        far: false,
                        never: false,
                        notreached: false,
                        cust_call: false,
                        noinline: false,
                        optdbginfo: false
                    },
                    name: "Baz::f_protected".into(),
                })
            );
        }

        #[test]
        fn kind_1103() {
            let data = &[
                3, 17, 244, 149, 9, 0, 40, 151, 9, 0, 135, 1, 0, 0, 108, 191, 184, 2, 1, 0, 0, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1103);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Block(BlockSymbol {
                    parent: SymbolIndex(0x0009_95f4),
                    end: SymbolIndex(0x0009_9728),
                    len: 391,
                    offset: PdbInternalSectionOffset {
                        section: 0x1,
                        offset: 0x02b8_bf6c
                    },
                    name: "".into(),
                })
            );
        }

        #[test]
        fn kind_110f() {
            let data = &[
                15, 17, 0, 0, 0, 0, 156, 1, 0, 0, 0, 0, 0, 0, 18, 0, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0,
                128, 16, 0, 0, 196, 87, 0, 0, 1, 0, 128, 95, 95, 115, 99, 114, 116, 95, 99, 111,
                109, 109, 111, 110, 95, 109, 97, 105, 110, 0, 0, 0,
            ];
            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x110f);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Procedure(ProcedureSymbol {
                    global: false,
                    dpc: false,
                    parent: None,
                    end: SymbolIndex(412),
                    next: None,
                    len: 18,
                    dbg_start_offset: 4,
                    dbg_end_offset: 9,
                    type_index: TypeIndex(4224),
                    offset: PdbInternalSectionOffset {
                        offset: 22468,
                        section: 1
                    },
                    flags: ProcedureFlags {
                        nofpo: false,
                        int: false,
                        far: false,
                        never: false,
                        notreached: false,
                        cust_call: false,
                        noinline: false,
                        optdbginfo: true
                    },
                    name: "__scrt_common_main".into(),
                })
            );
        }

        #[test]
        fn kind_1116() {
            let data = &[
                22, 17, 7, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 14, 0, 10, 0, 115, 98, 77, 105, 99,
                114, 111, 115, 111, 102, 116, 32, 40, 82, 41, 32, 76, 73, 78, 75, 0, 0, 0, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1116);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::CompileFlags(CompileFlagsSymbol {
                    language: SourceLanguage::Link,
                    flags: CompileFlags {
                        edit_and_continue: false,
                        no_debug_info: false,
                        link_time_codegen: false,
                        no_data_align: false,
                        managed: false,
                        security_checks: false,
                        hot_patch: false,
                        cvtcil: false,
                        msil_module: false,
                        sdl: false,
                        pgo: false,
                        exp_module: false,
                    },
                    cpu_type: CPUType::Intel80386,
                    frontend_version: CompilerVersion {
                        major: 0,
                        minor: 0,
                        build: 0,
                        qfe: None,
                    },
                    backend_version: CompilerVersion {
                        major: 14,
                        minor: 10,
                        build: 25203,
                        qfe: None,
                    },
                    version_string: "Microsoft (R) LINK".into(),
                })
            );
        }

        #[test]
        fn kind_1132() {
            let data = &[
                50, 17, 0, 0, 0, 0, 108, 0, 0, 0, 88, 0, 0, 0, 0, 0, 0, 0, 196, 252, 10, 0, 56, 67,
                0, 0, 1, 0, 1, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x1132);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::SeparatedCode(SeparatedCodeSymbol {
                    parent: SymbolIndex(0x0),
                    end: SymbolIndex(0x6c),
                    len: 88,
                    flags: SeparatedCodeFlags {
                        islexicalscope: false,
                        returnstoparent: false
                    },
                    offset: PdbInternalSectionOffset {
                        section: 0x1,
                        offset: 0xafcc4
                    },
                    parent_offset: PdbInternalSectionOffset {
                        section: 0x1,
                        offset: 0x4338
                    }
                })
            );
        }

        #[test]
        fn kind_113c() {
            let data = &[
                60, 17, 1, 36, 2, 0, 7, 0, 19, 0, 13, 0, 6, 102, 0, 0, 19, 0, 13, 0, 6, 102, 0, 0,
                77, 105, 99, 114, 111, 115, 111, 102, 116, 32, 40, 82, 41, 32, 79, 112, 116, 105,
                109, 105, 122, 105, 110, 103, 32, 67, 111, 109, 112, 105, 108, 101, 114, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x113c);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::CompileFlags(CompileFlagsSymbol {
                    language: SourceLanguage::Cpp,
                    flags: CompileFlags {
                        edit_and_continue: false,
                        no_debug_info: false,
                        link_time_codegen: true,
                        no_data_align: false,
                        managed: false,
                        security_checks: true,
                        hot_patch: false,
                        cvtcil: false,
                        msil_module: false,
                        sdl: true,
                        pgo: false,
                        exp_module: false,
                    },
                    cpu_type: CPUType::Pentium3,
                    frontend_version: CompilerVersion {
                        major: 19,
                        minor: 13,
                        build: 26118,
                        qfe: Some(0),
                    },
                    backend_version: CompilerVersion {
                        major: 19,
                        minor: 13,
                        build: 26118,
                        qfe: Some(0),
                    },
                    version_string: "Microsoft (R) Optimizing Compiler".into(),
                })
            );
        }

        #[test]
        fn kind_113e() {
            let data = &[62, 17, 193, 19, 0, 0, 1, 0, 116, 104, 105, 115, 0, 0];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x113e);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Local(LocalSymbol {
                    type_index: TypeIndex(5057),
                    flags: LocalVariableFlags {
                        isparam: true,
                        addrtaken: false,
                        compgenx: false,
                        isaggregate: false,
                        isaliased: false,
                        isalias: false,
                        isretvalue: false,
                        isoptimizedout: false,
                        isenreg_glob: false,
                        isenreg_stat: false,
                    },
                    name: "this".into(),
                })
            );
        }

        #[test]
        fn kind_114c() {
            let data = &[76, 17, 95, 17, 0, 0];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x114c);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::BuildInfo(BuildInfoSymbol {
                    id: IdIndex(0x115F)
                })
            );
        }

        #[test]
        fn kind_114d() {
            let data = &[
                77, 17, 144, 1, 0, 0, 208, 1, 0, 0, 121, 17, 0, 0, 12, 6, 3, 0,
            ];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x114d);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::InlineSite(InlineSiteSymbol {
                    parent: Some(SymbolIndex(0x0190)),
                    end: SymbolIndex(0x01d0),
                    inlinee: IdIndex(4473),
                    invocations: None,
                    annotations: BinaryAnnotations::new(&[12, 6, 3, 0]),
                })
            );
        }

        #[test]
        fn kind_114e() {
            let data = &[78, 17];

            let symbol = Symbol {
                data,
                index: SymbolIndex(0),
            };
            assert_eq!(symbol.raw_kind(), 0x114e);
            assert_eq!(symbol.parse().expect("parse"), SymbolData::InlineSiteEnd);
        }
    }

    mod iterator {
        use fallible_iterator::FallibleIterator;
        use crate::{*, symbol::{types::*, *}};

        fn create_iter() -> SymbolIter<'static> {
            let data = &[
                0x00, 0x00, 0x00, 0x00, // module signature (padding)
                0x02, 0x00, 0x4e, 0x11, // S_INLINESITE_END
                0x02, 0x00, 0x06, 0x00, // S_END
            ];

            let mut buf = ParseBuffer::from(&data[..]);
            buf.seek(4); // skip the module signature
            SymbolIter::new(buf)
        }

        #[test]
        fn test_iter() {
            let symbols: Vec<_> = create_iter().collect().expect("collect");

            let expected = [
                Symbol {
                    index: SymbolIndex(0x4),
                    data: &[0x4e, 0x11], // S_INLINESITE_END
                },
                Symbol {
                    index: SymbolIndex(0x8),
                    data: &[0x06, 0x00], // S_END
                },
            ];

            assert_eq!(symbols, expected);
        }

        #[test]
        fn test_seek() {
            let mut symbols = create_iter();
            symbols.seek(SymbolIndex(0x8));

            let symbol = symbols.next().expect("get symbol");
            let expected = Symbol {
                index: SymbolIndex(0x8),
                data: &[0x06, 0x00], // S_END
            };

            assert_eq!(symbol, Some(expected));
        }

        #[test]
        fn test_skip_to() {
            let mut symbols = create_iter();
            let symbol = symbols.skip_to(SymbolIndex(0x8)).expect("get symbol");

            let expected = Symbol {
                index: SymbolIndex(0x8),
                data: &[0x06, 0x00], // S_END
            };

            assert_eq!(symbol, Some(expected));
        }
    }
}
