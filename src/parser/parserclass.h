
#include <string>

using namespace std;

class Parser {
    public:
	Parser(std::string filename);
	~Parser();
	void parse();
	void execute();

    private:
	std::string filename;
	
};

