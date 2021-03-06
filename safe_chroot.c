#include <stdio.h>
#include <unistd.h>

/* minimalistic chroot */

int main(int argc, char* argv[])
{
    if (argc < 3)
    {
        //printf("%s NEWROOT COMMAND [ARG]", argv[0]);
        printf("Failed to launch process...\n");
        return 0;
    }

    if (chdir(argv[1]) < 0)
    {
        printf("Failed to launch process...\n");
        //perror("chdir failed");
        return 1;
    }

    if (chroot(argv[1]) < 0)
    {
        printf("Failed to launch process...\n");
        //perror("chroot failed");
        return 1;
    }
    if (setuid(42) < 0)
    {
        printf("Failed to launch process...\n");
        //perror("setuid failed");
        return 1;
    }
    chdir("/room/");
    execvp(argv[2], argv+2);
    return 0;
}
