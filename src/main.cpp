#include "Parser.hpp"
#include <fstream>

// Use tutorials in: https://llvm.org/docs/tutorial/

int main(int argc, char * argv[]) {
    bool ret_val = false;

    std::string infile = argc > 1 ? argv[1] : "";
    std::ifstream ifs(infile);

    std::istream & in = argc > 1 ? ifs : std::cin;

    if (argc > 1 && !ifs) {
            std::cerr << "\033[31m"
                      << "[ERROR] "
                      << "\033[m"
                      << "Couldn't open file: " << argv[1] << std::endl;
            return EXIT_FAILURE;
    }

    Parser parser(in);
    ret_val = parser.Parse();

    if (!ret_val) {
        return EXIT_FAILURE;
    }
    parser.Generate().print(llvm::outs(), nullptr);

    return EXIT_SUCCESS;
}
