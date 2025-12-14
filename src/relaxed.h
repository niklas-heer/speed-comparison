static inline double relaxed_add(double a, double b) {
    _Pragma("clang fp reassociate(on) contract(fast)")
    return a + b;
}
