pub const DEF_WORLD_WIDTH: u32 = 1496;
pub const DEF_WORLD_HEIGHT: u32 = 1024;

#[derive(Copy, Clone, Debug)]
pub struct WorldSize(pub u32, pub u32);

impl Default for WorldSize {
    fn default() -> Self {
        WorldSize(DEF_WORLD_WIDTH, DEF_WORLD_HEIGHT)
    }
}
