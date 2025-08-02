CRAFTING_INTERPRETERS_REPO_REL_PATH="../craftinginterpreters"
LOCAL_RUST_BIN="../rox/target/debug/rox"


test_to_run="chap04_scanning"

cargo build
cd ../craftinginterpreters/
dart tool/bin/test.dart java scanning --interpreter "$LOCAL_RUST_BIN"  
