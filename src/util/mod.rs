mod span;

use std::sync::Arc;

pub use span::Span;

use codespan_reporting::files::{Files, SimpleFiles};

pub type FileId = <SimpleFiles<String, String> as Files<'static>>::FileId;
pub type P<T> = Arc<T>;
