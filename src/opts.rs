use clap::Clap;

#[derive(Debug, Clap)]
#[clap(author, about, version)]
pub struct Opts {
    #[clap(short, long, takes_value = true, conflicts_with = "entry")]
    pub code: Option<String>,
    #[clap(required_unless_present = "code", index = 1)]
    pub entry: Option<String>,
    #[clap(short, long, takes_value = false)]
    pub verbose: bool,
    #[clap(alias = "objf", long = "object-file")]
    pub objfile: Option<Option<String>>,
    #[clap(short, long, takes_value = false)]
    pub run: bool,
}
