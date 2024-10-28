#include <cstdio>
#include <cstdint>


const uint16_t bufsize=0x4000;

int main( int argc, char *argv[] ) {
    FILE *in;
    int romsize, maxsize;
    uint8_t rom[ bufsize ] = {0};
    if( argc < 3 ) {
        fprintf( stderr, "usage: obj2h <OBJFILE>\n" );
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
    printf( "#ifndef ROM_B1K_H\n#define ROM_B1K_H\n\n" );
    printf( "const unsigned char rom_b1k[] PROGMEM = {" );

    for ( int iii = 0; iii < romsize; ++iii ) {
        if ( (iii & 0x0F) == 0 )
            printf( "\n    /* %04x */", iii );
        printf( " 0x%02x%c", rom[ iii ], iii == romsize - 1 ? '\n' : ',' );
    }
    printf( "};\n\n" );
    printf( "#endif\n" );
    return 0;
}
