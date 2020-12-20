const c = @cImport({
    @cInclude("sincos.h");
});

pub inline fn sincosf(a: f32, sin: *f32, cos: *f32) void {
    c.sincosf(a, sin, cos);
}
