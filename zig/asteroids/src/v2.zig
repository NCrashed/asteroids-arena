// 2D vector of floats
pub const Vec2 = struct {
    x: f32,
    y: f32,

    /// Add components of second vector to the firsts one
    pub fn add(self: *Vec2, v: Vec2) *Vec2 {
        self.x += v.x;
        self.y += v.y;
        return self;
    }

    /// Rotate the vector around 0 to given angle
    pub fn rotate(self: *Vec2, angle: f32) *Vec2 {
        const x = self.x * @cos(angle) - self.y * @sin(angle);
        const y = self.x * @sin(angle) - self.y * @cos(angle);
        self.x = x;
        self.y = y;
        return self;
    }

    /// Return squared distance between two points. Used for collision detection.
    pub fn dist_squared(self: Vec2, v: Vec2) f32 {
        const x = self.x - v.x;
        const y = self.y - v.y;
        return x*x + y*y;
    }

    /// Scale given vector by scalar
    pub fn scale(self: *Vec2, scalar: f32) *Vec2 {
        self.x *= scalar;
        self.y *= scalar;
    }
};
