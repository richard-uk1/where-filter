use where_filter::Query;

fn main() {
    println!("{:?}", Query::parse("x = 2 and y rlike 'txt'"));
}
