#ifndef _FUNCTOR_H_INC_
#define _FUNCTOR_H_INC_

#include <cctype>
#include <string>
#include <functional>

// nullfunctor is a functor that does nothing, allows usage of shared_ptr with stack allocated or static objects.
// Taken from boost/serialization/shared_ptr.hpp

template<typename T>
struct null_unary_functor
    : public std::unary_function<T*, void>
{
    void operator()(T const*) const {}
};

template<typename T>
struct null_binary_functor
    : public std::binary_function<T*, T*, void>
{
    void operator()(T const*, T const*) const {}
};

// Case-insensitive comparator for char

inline bool no_case_less(unsigned char const c1, unsigned char const c2)
{
    return
        //toupper(c1) < toupper(c2);
        tolower(c1) < tolower(c2);
}

inline bool no_case_more(unsigned char const c1, unsigned char const c2)
{
    return
        //toupper(c1) > toupper(c2);
        tolower(c1) > tolower(c2);
}

inline bool no_case_equal(unsigned char const c1, unsigned char const c2)
{
    return
        //toupper(c1) == toupper(c2);
        tolower(c1) == tolower(c2);
}

// Case-insensitive comparator for string

struct no_case_less_comparer
    : public std::binary_function<std::string&, std::string&, bool>
{
    bool operator()(std::string const &s1, std::string const &s2) const
    {
        //return stricmp(s1.c_str(), s2.c_str()) < 0;
        return lexicographical_compare(s1.begin(), s1.end(), s2.begin(), s2.end(), no_case_less);
    }
};

struct no_case_more_comparer
    : public std::binary_function<std::string&, std::string&, bool>
{
    bool operator()(std::string const &s1, std::string const &s2) const
    {
        //return stricmp(s1.c_str(), s2.c_str()) > 0;
        return std::lexicographical_compare(s1.begin(), s1.end(), s2.begin(), s2.end(), no_case_more);
    }
};

struct no_case_equal_comparer
    : public std::binary_function<std::string&, std::string&, bool>
{
    bool operator()(std::string const &s1, std::string const &s2) const
    {
        //return stricmp(s1.c_str(), s2.c_str()) == 0;
        return std::lexicographical_compare(s1.begin(), s1.end(), s2.begin(), s2.end(), no_case_equal);
    }
};

#endif // _FUNCTOR_H_INC_
