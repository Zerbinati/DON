#ifndef _MANIPULATOR_H_INC_
#define _MANIPULATOR_H_INC_

#include <iostream>

namespace std {

    class width_prec
    {

    private:
        int width;
        int prec;

    public:

        width_prec (int w, int p)
            : width (w)
            , prec (p)
        {}

        template<typename CharT, typename Traits>
        basic_ostream<CharT, Traits>& operator() (basic_ostream<CharT, Traits> &os) const
        {
            os.setf (ios_base::fixed, ios_base::floatfield);
            os.width (width);
            os.precision (prec);
            return os;
        }

    };

    template<typename CharT, typename Traits>
    inline basic_ostream<CharT, Traits>&
        operator<< (basic_ostream<CharT, Traits> &os, const width_prec &wp)
    {
        return wp (os);
    }

    /*
    // manip_infra is a small, intermediary class that serves as a utility
    // for custom manipulators with arguments.
    // Call its constructor with a function pointer and a value
    // from your main manipulator function.
    // The function pointer should be a helper function that does the actual work.
    // See examples below.
    template<typename T, typename C>
    class manip_infra
    {

    private:

        basic_ostream<C>& (*manip_fp) (basic_ostream<C>&, T);
        
        T value;

    public:
        manip_infra (basic_ostream<C>& (*man_fp) (basic_ostream<C>&, T), T val)
            : manip_fp (man_fp)
            , value (val)
        {}

        void operator() (basic_ostream<C> &os) const
        {
            // Invoke the function pointer with the stream and value
            manip_fp (os, value);
        }  

    };

    template<typename T, typename C>
    inline basic_ostream<C>& operator<< (basic_ostream<C> &os, const manip_infra<T, C> &manip)
    {
        manip (os);
        return os;
    }

    // Helper function that is ultimately called by the ManipInfra class
    inline ostream& set_width (ostream &os, int n)
    {
        os.width (n);
        return (os);
    }

    // Manipulator function itself. This is what is used by client code
    inline manip_infra<int, char> set_width (int n)
    {
        return (manip_infra<int, char> (set_width, n));
    }

    // Another helper that takes a char argument
    inline ostream& set_fill (ostream &os, char c)
    {
        os.fill (c);
        return (os);
    }

    inline manip_infra<char, char> set_fill (char c)
    {
        return (manip_infra<char, char> (set_fill, c));
    }
    */
}

#endif // _MANIPULATOR_H_INC_
