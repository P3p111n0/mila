#include "Parser.hpp"
#include <fstream>

// Use tutorials in: https://llvm.org/docs/tutorial/

int main(int argc, char * argv[]) {
    bool ret_val = false;

    if (argc > 1) {
        std::ifstream ifs(argv[1]);
        if (!ifs) {
            std::cerr << "\033[31m" << "[ERROR] " << "\033[m" << "Couldn't open file: " << argv[1] << std::endl;
            return 1;
        }
        Parser parser(ifs);
        ret_val = parser.Parse();
    } else {
        Parser parser;
        ret_val = parser.Parse();
    }

    if (!ret_val) {
        return EXIT_FAILURE;
    }

    // parser.Generate().print(llvm::outs(), nullptr);

    return EXIT_SUCCESS;
}
