#include <cctype>
#include <cstdio>
#include <cstdint>


const uint16_t bufsize=0x4000;

void strtolower(char* str) {
    while (*str) { *(str) = tolower((unsigned char)*str); ++str; }
}

void strtoupper(char* str) {
    while (*str) { *(str) = toupper((unsigned char)*str); ++str; }
}

int main( int argc, char *argv[] ) {
    FILE *in;
    int romsize, maxsize;
    uint8_t rom[ bufsize ] = {0};
    if( argc < 4 ) {
        fprintf( stderr, "usage: obj2h <OBJFILE> <ROMSIZE> <ROMNAME>\n" );
        return -1;
    }
    if ( 1 != sscanf( argv[2], "0x%X", &maxsize ) )
        return -1;
    in = fopen( argv[1], "rb" );
    if( !in ) {
        fprintf( stderr, "error: cannot open %s\n", argv[1] );
        return -1;
    }
    romsize = fread( rom, 1, sizeof( rom ), in );
    fclose( in );
    if ( romsize > maxsize ) {
        fprintf( stderr, "\nERROR: size of %s (0x%04X) is bigger than the limit (0x%04X)\n\n",
                 argv[1], romsize, maxsize );
        return -1;
    }
    strtoupper( argv[3] );
    printf( "#ifndef %s_H\n#define %s_H\n\n", argv[3], argv[3] );
    strtolower( argv[3] );
    printf( "const unsigned char %s[] PROGMEM = {", argv[3] );

    for ( int iii = 0; iii < romsize; ++iii ) {
        if ( (iii & 0x0F) == 0 )
            printf( "\n    /* %04x */", iii );
        printf( " 0x%02x%c", rom[ iii ], iii == romsize - 1 ? '\n' : ',' );
    }
    printf( "};\n\n" );
    printf( "#endif\n" );
    return 0;
}
