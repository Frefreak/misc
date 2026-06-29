use std::path::Path;

use swc_common::sync::Lrc;
use swc_common::{
    errors::{ColorConfig, Handler},
    SourceMap,
};
use swc_ecma_codegen::Emitter;

use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};

fn main() {
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));

    let input_file = std::env::args().nth(1).unwrap_or_else(|| {
        panic!(
            "Usage: {} <input.js> <output.js>",
            std::env::args().next().unwrap()
        )
    });
    let output_file = std::env::args().nth(2).unwrap_or_else(|| {
        panic!(
            "Usage: {} <input.js> <output.js>",
            std::env::args().next().unwrap()
        )
    });
    let fm = cm
        .load_file(Path::new(&input_file))
        .unwrap_or_else(|_| panic!("failed to load {}", input_file));
    let lexer = Lexer::new(
        // We want to parse ecmascript
        Syntax::Es(Default::default()),
        // EsVersion defaults to es5
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let module = parser
        .parse_module()
        .map_err(|e| {
            // Unrecoverable fatal error occurred
            e.into_diagnostic(&handler).emit()
        })
        .expect("failed to parser module");
    let mut buf = vec![];
    let write_js = swc_ecma_codegen::text_writer::JsWriter::new(cm.clone(), "\n", &mut buf, None);
    let mut emitter = Emitter {
        cfg: Default::default(),
        comments: None,
        cm: cm.clone(),
        wr: Box::new(write_js),
    };
    emitter.emit_module(&module).expect("emit module error");
    std::fs::write(output_file, buf).expect("write failed");
}
