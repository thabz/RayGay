
#ifndef RAYGAY_TESTING_H
#define RAYGAY_TESTING_H

#include <vector>
#include <string>

using namespace std;

#undef assertTrue
#define assertTrue(e) (_assertTrue((e),__FILE__,__LINE__,#e))

class Test {

    public:
	Test(string name);
	virtual ~Test() {};
	virtual void run() = 0;
	void _assertTrue(bool expr, char* filename, int line, char* expr_code);
	void printStatus();
	bool hasFailures();

    private:
	string name;
	int total_asserts;
	int failed_asserts;
	int succeded_asserts;
	vector<string> failed_output;
};

class TestSuite {

    public:
	void add(Test* test);
	void run();
	void printStatus();
	bool hasFailures();
	
    private:
	vector<Test*> tests;
};

void TestSuite::run() {
    for(unsigned int i = 0; i < tests.size(); i++) {
	tests[i]->run();
    }
}

void TestSuite::printStatus() {
    for(unsigned int i = 0; i < tests.size(); i++) {
	cout << "   ";
	tests[i]->printStatus();
    }
}

bool TestSuite::hasFailures() {
    bool result = false;
    for(unsigned int i = 0; i < tests.size(); i++) {
	result |= tests[i]->hasFailures();
    }
    return result;
}

void TestSuite::add(Test* test) {
    tests.push_back(test);
}

Test::Test(string name) {
    this->name = name;
    total_asserts = 0;
    failed_asserts = 0;
    succeded_asserts = 0;
}

void Test::_assertTrue(bool expr, char* filename, int line, char* expr_code) {
    total_asserts++;
    if (expr) {
	succeded_asserts++;
    } else {
	char line_c[10];
	sprintf(line_c,"%d",line);
	string error_text = "Failed test: ";
	error_text += string(filename);
	error_text += ":";
	error_text += string(line_c);
	error_text += ":";
	error_text += string(expr_code);
	failed_output.push_back(error_text);
	failed_asserts++;
    }
}

void Test::printStatus() {
    cout << (failed_asserts > 0 ? "Failure for " : "Success for ") << name << ": ";
    cout << succeded_asserts << "/" << total_asserts << endl;
    if (failed_asserts > 0) {
	for(unsigned int i = 0; i < failed_output.size(); i++) {
	    cout << "      ";
	    cout << failed_output[i] << endl;
	}
    }
}

bool Test::hasFailures() {
    return failed_asserts > 0;
}

#endif
