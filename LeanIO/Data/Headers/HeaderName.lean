module

public import Std.Http.Data.Headers.Name

namespace Std.Http.Header.Name

public def contentDisposition := Header.Name.mk "content-disposition"
public def acceptRanges       := Header.Name.mk "accept-ranges"
public def acceptEncoding     := Header.Name.mk "accept-encoding"
public def contentRange       := Header.Name.mk "content-range"
public def range              := Header.Name.mk "range"
public def wwwAuthenticate    := Header.Name.mk "www-authenticate"
public def cacheControl       := Header.Name.mk "cache-control"
public def etag               := Header.Name.mk "etag"

end Std.Http.Header.Name
