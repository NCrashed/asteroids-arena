const math = @import("math.zig");
const sincosf = math.sincosf;

// 2D vector of floats
pub const Vec2 = struct {
    x: f32,
    y: f32,

    /// Add components of second vector to the firsts one
    pub inline fn add(self: *Vec2, v: Vec2) *Vec2 {
        self.x += v.x;
        self.y += v.y;
        return self;
    }

    /// Substract components of second vector from the firsts one
    pub inline fn sub(self: *Vec2, v: Vec2) *Vec2 {
        self.x -= v.x;
        self.y -= v.y;
        return self;
    }

    /// Rotate the vector around 0 to given angle
    pub inline fn rotate(self: *Vec2, angle: f32) *Vec2 {
        var sina: f32 = undefined;
        var cosa: f32 = undefined;
        sincosf(angle, &sina, &cosa);
        const x = self.x * cosa - self.y * sina;
        const y = self.x * sina + self.y * cosa;
        self.x = x;
        self.y = y;
        return self;
    }

    /// Return squared distance between two points. Used for collision detection.
    pub inline fn dist_squared(self: Vec2, v: Vec2) f32 {
        const x = self.x - v.x;
        const y = self.y - v.y;
        return x * x + y * y;
    }

    /// Scale given vector by scalar
    pub inline fn scale(self: *Vec2, scalar: f32) *Vec2 {
        self.x *= scalar;
        self.y *= scalar;
        return self;
    }
};
