class ParamTouzaNio:
    NIO_HEADER_ITEMS = 64
    NIO_HEADER_LEN_ITEM = 16
    NIO_CACHE_COLL_DEFAULT = 0
    NIO_CACHE_COLL_MASK_STD = 1
    NIO_CACHE_COLL_MASK_BASIC = 2
    NIO_CACHE_COLL_MASK_NOSIGN = 4
    NIO_CACHE_COLL_MASK_NONUM = 8
    NIO_CACHE_COLL_STD = 1
    NIO_CACHE_COLL_BASIC = (1+2)
    NIO_CACHE_COLL_NOSIGN = (1+2+4)
    NIO_CACHE_COLL_NONUM = (1+2+4+8)
    NIO_CACHE_COLL_NOSPECIAL = 16
    NIO_CACHE_ALLOW_VAR_DUP = 32
    NIO_CACHE_ALLOW_GRP_DUP = 64
    NIO_CACHE_ALLOW_COOR_DUP = 128
    NIO_CONTROL_ENABLE_CACHE = 256
    NIO_CONTROL_ENABLE_SEQUENTIAL = 512
    NIO_CONTROL_ENABLE_AUTO = 1024
    NIO_CACHE_VAR_SUITE = -99

