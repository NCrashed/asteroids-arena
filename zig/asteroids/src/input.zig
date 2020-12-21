/// Only object that is controlled by the player is space ship. We consider
/// input events to be "sticky". Ship fires all time while key is down.
pub const Events = struct {
    ship_left: bool,
    ship_right: bool,
    ship_thrust: bool,
    ship_fire: bool,

    /// Setup default values for input events
    pub fn init() Events {
        return Events{
            .ship_left = false,
            .ship_right = false,
            .ship_thrust = false,
            .ship_fire = false,
        };
    }
};
