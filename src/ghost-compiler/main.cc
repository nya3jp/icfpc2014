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
        return "";

    string::size_type right = str.find_last_not_of(" \t\r\n");
    return str.substr(left, right - left + 1);
}

static string stripComment(string s)
{
    string::size_type pos = s.find_first_of(";");
    if (pos == string::npos)
        return trim(s);

    return trim(s.substr(0, pos));
}

static string stripWhiteSpace(string s)
{
    string result;
    for (char c : s) {
        if (c == ' ' || c == '\t')
            continue;
        result += c;
    }
    return result;
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

string toUpper(string s) {
    for (string::size_type i = 0; i < s.size(); ++i) {
        if ('a' <= s[i] && s[i] <= 'z') {
            s[i] -= 'a' - 'A';
        }
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

            if (label.size() <= 2) {
                cerr << "Please use more than 3 letters for label: " << label << endl;
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
        if (line[0]  == ';') {
            cout << line << endl;
            continue;
        }

        string trimed = trim(line);
        string::size_type pos = trimed.find(' ');
        if (pos == string::npos) {
            cout << line << endl;
            continue;
        }

        string fst = trimed.substr(0, pos);
        string snd = stripWhiteSpace(trimed.substr(pos + 1));
        fst = toUpper(fst);

        if (fst != "JLT" && fst != "JEQ" && fst != "JGT" && fst != "MOV") {
            cout << line << endl;
            continue;
        }

        if (fst == "MOV") {
            string::size_type pos = snd.find(',');
            if (pos == string::npos) {
                cerr << "invalid instruction?" << endl;
                return 1;
            }

            string labelName = trim(snd.substr(pos + 1));
            if (pcMap.count(labelName) > 0) {
                string replaced = replaceString(line, labelName, to_string(pcMap[labelName]));
                cout << replaced << "\t\t;" << trim(line) << endl;
            } else {
                cout << line << endl;
            }

        } else {
            string::size_type pos = snd.find(',');
            if (pos == string::npos) {
                cerr << "invalid instruction?" << endl;
                return 1;
            }

            string labelName = snd.substr(0, pos);
            if (pcMap.count(labelName) > 0) {
                cout << replaceString(line, labelName, to_string(pcMap[labelName]))
                     << "\t\t;" << trim(line) << endl;
            } else {
                cout << line << endl;
            }
        }
    }

    return 0;
}
