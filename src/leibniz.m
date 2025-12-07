#import <Foundation/Foundation.h>
#import <stdio.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"rounds.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        long rounds = [content integerValue];

        double x = 1.0;
        double pi = 1.0;

        for (long i = 2; i <= rounds + 2; i++) {
            x *= -1.0;
            pi += x / (2.0 * i - 1.0);
        }

        pi *= 4.0;

        printf("%.17g", pi);
    }
    return 0;
}
