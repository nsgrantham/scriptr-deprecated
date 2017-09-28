#include <stdlib.h>
#include <getopt.h>
#include <memory>
#include <string>
#include <map>

#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

// alias for C getopt parameter structs
typedef struct option longopt;

// opts is a list of lists with same elements as libc's [struct option]:
//   - name, String       // const char *name;
//   - argument, int      // int has_arg;
//   - value              // int val;
//
// TODO: add POSIXLY_CORRECT argument and similar (adds "-" to start of opt string)
//
// [[Rcpp::export]]
List callgetopt(CharacterVector args, ListOf<List> opts) {
  List ret; // we'll return a list of arguments
  int optc = opts.size();

  // c-style argc and argv
  int argc = args.size();
  std::unique_ptr<char*[]> argv(new char*[argc]);

  // convert opts to required structures
  std::unique_ptr<longopt[]> optargs(new longopt[optc+1]);

  // mapping from equivalent short->long options (single character string
  // to 0-based index of long option struct in optargs array)
  map<string, int> shortLongEquiv;

  // somewhere to keep c++ strings (which contain internal C strings)
  // so they stay in memory until the function returns
  vector<string> stringvec(argc);
  for (int i=0; i<argc; i++) {
    const char *argstr = CHAR(STRING_ELT(args, i));
    argv[i] = (char *)argstr;
  }

  // container for C name strings, which we'll clean up below
  // TODO: replace with unique_ptr so it cleans up automatically
  vector<char *> names;

  string shortString("");

  for (int i=0; i<optc; i++) {
    List thisOpt = opts[i];
    //CharacterVector cv_name = as<CharacterVector>(thisOpt["name"]);
    //const char *cname = CHAR(STRING_ELT(cv_name, 1));
    string namecpp = Rcpp::as<string>(thisOpt["name"]);
    names.push_back(strdup(namecpp.c_str()));
    optargs[i].name = names[i];
    string opttype = Rcpp::as<string>(thisOpt["opttype"]);
    string shortSuffix = "";
    if (opttype == "required") {
      optargs[i].has_arg = required_argument;
      shortSuffix = ":";
    } else if (opttype == "flag") {
      optargs[i].has_arg = no_argument;
    } else {
      optargs[i].has_arg = optional_argument;
      shortSuffix = ":";
    }
    optargs[i].flag = NULL;
    optargs[i].val = i;
    string shortOpt = Rcpp::as<string>(thisOpt["short"]);
    if (shortOpt != "") {
      shortLongEquiv[shortOpt] = i;
      shortString += shortOpt + shortSuffix;
    }
  }
  optargs[optc].name = NULL;
  optargs[optc].has_arg = 0;
  optargs[optc].flag = NULL;
  optargs[optc].val = 0;

  bool error = false;  // user gets this as well as (partial) results

  // parse from first element (non-standard; it's usually 1)
  optind = 1;
  char c;
  while (true) {
    int option_index = -1;
    c = getopt_long(argc, argv.get(), shortString.c_str(),
                    optargs.get(), &option_index);
    if (c == -1)
      break;
    if (c == '?') {
      error = true;
      continue; // try to keep parsing
    }
    string val = "";
    if (optarg != NULL) {
      val = string(optarg);
    }
    if (option_index == -1) {
      // matched short option
      string key(1, c);
      auto equivLong = shortLongEquiv.find(key);
      if (equivLong != shortLongEquiv.end()) {
        List longOpt = opts[equivLong->second];
        key = Rcpp::as<string>(longOpt["name"]);
      }
      ret[key] = val;
    } else {
      // matched long option; index applies to original list
      string key = Rcpp::as<string>(opts[option_index]["name"]);
      ret[key] = val;
    }
  }

  for (auto n : names) {
    free(n);
  }

  if (error) {
    ret.attr("error") = true;
  }

  return ret;
}
