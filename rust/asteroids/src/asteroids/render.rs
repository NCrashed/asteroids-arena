use sdl2::pixels::Color;
use sdl2::rect::Point;
use sdl2::render::*;
use specs::prelude::*;
use glam::Vec2;
use glam::Mat3;

use super::components::player::*;
use super::components::pos::*;
use super::components::rot::*;

pub type SystemData<'a> = (
    ReadStorage<'a, Pos>,
    ReadStorage<'a, Rot>,
    ReadStorage<'a, Player>);

pub fn render_world(canvas: &mut WindowCanvas, (pos, rot, player): SystemData) -> Result<(), String> {
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    for (player, pos, rot) in (&player, &pos, &rot).join() {
        render_player(canvas, player, pos, rot)?;
    }
    canvas.present();
    Ok(())
}

fn transform(mat: &Mat3, vs: &mut [Point]) {
    vs.iter_mut().for_each(|v| {
        let tv = mat.transform_point2(Vec2::new(v.x as f32, v.y as f32));
        *v = Point::new(tv.x as i32, tv.y as i32);
    });
}

fn render_player(canvas: &mut WindowCanvas, player: &Player, pos: &Pos, rot: &Rot) -> Result<(), String> {
    let dx = (PLAYER_SIZE.x * 0.5) as i32;
    let dy = (PLAYER_SIZE.y * 0.5) as i32;
    canvas.set_draw_color(Color::RGB(255, 255, 255));
    let mat = Mat3::from_scale_angle_translation(Vec2::new(1.0, 1.0), rot.0, pos.0);
    let mut points = [
          Point::new(dx, 0)
        , Point::new(-dx, -dy)
        , Point::new(-dx, dy)
        , Point::new(dx, 0)
        ];
    transform(&mat, &mut points[..]);
    canvas.draw_lines(&points[..])?;

    if player.0 {
        let mut points = [
              Point::new(-dx, dy / 2)
            , Point::new(-dx-10, 0)
            , Point::new(-dx, -dy / 2)
            ];
        transform(&mat, &mut points[..]);
        canvas.draw_lines(&points[..])?;
    }
    Ok(())
}
