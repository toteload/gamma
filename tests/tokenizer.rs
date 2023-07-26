use gamma::string_interner::StringInterner;
use gamma::tokenizer::Tokenizer;
use insta::assert_ron_snapshot;

#[test]
fn tokenizer_creates_the_expected_tokens() {
    let source = "fn if else + - * ; : { } ( ) void let voidlet 4687 continue return->,=       
int &&& == ^ & | || !! / != >= <= < > true false loop break bool int void ";

    let mut symbols = StringInterner::new();
    let tokenizer = Tokenizer::new(source, &mut symbols);

    let tokens = tokenizer.collect::<Vec<_>>();

    assert_ron_snapshot!("tokens_snapshot", tokens);
}
