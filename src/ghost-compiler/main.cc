// ghost compiler is just convert LABEL to the program counter.

// g++ -std=c++11 main.cc -o ghostcompiler
// ghostcompiler < hoge.ghc

// LABEL should be all-capital, and end with:
// LABEL:
// label line should not contain any other things. (OK for comment)

// e.g.
// HOGE:
//     MOV a,1
// FUGA:
//     JGT HOGE,a,b
//
// will be converted to
//
// ;HOGE
//     MOV a,1
// ;FUGA
//     JGT 0,a,b        ;JGT HOGE,a,b
//     JGT 1,a,b        ;JGT FUGA,a,b

#include <iostream>
#include <string>
#include <sstream>
#include <map>
#include <vector>

using namespace std;

static string trim(const string& str)
{
    string::size_type left = str.find_first_not_of(" \t\r\n");
    if (left == string::npos)
        return str;

    string::size_type right = str.find_last_not_of(" \t\r\n");
    return str.substr(left, right - left + 1);
}

static string stripComment(string s)
{
    string::size_type pos = s.find_first_of(";");
    if (pos == string::npos)
        return s;

    return trim(s.substr(0, pos));
}

static bool isAllCapital(const string& s)
{
    for (char c : s) {
        if ('A' <= c && c <= 'Z')
            continue;
        if ('0' <= c && c <= '9')
            continue;
        if (c == '_')
            continue;
        return false;
    }

    return true;
}

string replaceString(string s, const string& from, const string& to)
{
    string::size_type pos = s.find(from);
    while (pos != string::npos) {
        s.replace(pos, from.size(), to);
        pos = s.find(from, pos + to.size());
    }

    return s;
}

int main(void)
{
    string str;
    int pc = 0;
    map<string, int> pcMap;
    vector<string> lines;

    while (getline(cin, str)) {
        str = stripComment(str);
        if (str.empty())
            continue;

        if (str[str.size() - 1] == ':') {
            string label = str.substr(0, str.size() - 1);
            // this looks a label.
            if (pcMap.count(label)) {
                cerr << "LABEL DUPLICATE!: " << label
                     << " the previous label pc was: " << pcMap[label] << endl;
                return 1;
            }

            if (!isAllCapital(label)) {
                cerr << "LABEL should be all capital: " << label << endl;
                return 1;
            }

            // OK
            pcMap[label] = pc;
            lines.push_back(";" + label);
            continue;
        }

        lines.push_back(str);
        ++pc;
    }

    for (const auto& line : lines) {
        stringstream ss(line);
        string fst;
        ss >> fst;

        if (fst != "JLT" && fst != "JEQ" && fst != "JGT" && fst != "MOV") {
            cout << line << endl;
            continue;
        }

        if (fst == "MOV") {
            string snd;
            ss >> snd;
            string::size_type pos = snd.find(',');
            if (pos == string::npos) {
                cerr << "invalid instruction?" << endl;
                return 1;
            }

            string labelName = trim(snd.substr(pos + 1));
            cout << replaceString(line, labelName, to_string(pcMap[labelName]))
                 << "\t\t;" << trim(line) << endl;

        } else {
            string snd;
            ss >> snd;
            string::size_type pos = snd.find(',');
            if (pos == string::npos) {
                cerr << "invalid instruction?" << endl;
                return 1;
            }

            string labelName = snd.substr(0, pos);
            cout << replaceString(line, labelName, to_string(pcMap[labelName]))
                 << "\t\t;" << trim(line) << endl;
        }
    }

    return 0;
}
