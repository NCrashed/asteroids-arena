use glam::Mat3;
use glam::Vec2;
use legion::*;
use legion::world::SubWorld;
use sdl2::pixels::Color;
use sdl2::rect::Point;
use sdl2::render::*;
use std::f32::consts::PI;

use super::super::components::asteroid::*;
use super::super::components::bullet::*;
use super::super::components::player::*;
use super::super::components::pos::*;
use super::super::components::rot::*;

#[system]
#[read_component(Player)]
#[read_component(Bullet)]
#[read_component(Asteroid)]
#[read_component(Pos)]
#[read_component(Rot)]
pub fn render(
    #[resource] canvas: &mut WindowCanvas,
    world: &SubWorld,
) {
    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();

    let mut player_query = <(&Player, &Pos, &Rot)>::query();
    for (player, pos, rot) in player_query.iter(world) {
        render_player(canvas, player, pos, rot);
    }
    let mut bullet_query = <(&Bullet, &Pos)>::query();
    for (_, pos) in bullet_query.iter(world) {
        render_bullet(canvas, pos);
    }
    let mut asteroid_query = <(&Asteroid, &Pos, &Rot)>::query();
    for (asteroid, pos, rot) in asteroid_query.iter(world) {
        render_asteroid(canvas, asteroid, pos, rot);
    }
    canvas.present();
}

fn transform(mat: &Mat3, vs: &mut [Point]) {
    vs.iter_mut().for_each(|v| {
        let tv = mat.transform_point2(Vec2::new(v.x as f32, v.y as f32));
        *v = Point::new(tv.x as i32, tv.y as i32);
    });
}

fn render_player(
    canvas: &mut WindowCanvas,
    player: &Player,
    pos: &Pos,
    rot: &Rot,
) {
    let dx = (PLAYER_SIZE.x * 0.5) as i32;
    let dy = (PLAYER_SIZE.y * 0.5) as i32;
    canvas.set_draw_color(Color::RGB(255, 255, 255));
    let mat = Mat3::from_scale_angle_translation(Vec2::new(1.0, 1.0), rot.0, pos.0);
    let mut points = [
        Point::new(dx, 0),
        Point::new(-dx, -dy),
        Point::new(-dx, dy),
        Point::new(dx, 0),
    ];
    transform(&mat, &mut points[..]);
    canvas.draw_lines(&points[..]).unwrap();

    if player.0 {
        let mut points = [
            Point::new(-dx, dy / 2),
            Point::new(-dx - 10, 0),
            Point::new(-dx, -dy / 2),
        ];
        transform(&mat, &mut points[..]);
        canvas.draw_lines(&points[..]).unwrap();
    }
}

fn render_bullet(canvas: &mut WindowCanvas, pos: &Pos) {
    canvas.draw_point(Point::new(pos.0.x as i32, pos.0.y as i32)).unwrap()
}

fn circloid_point(i: u32, n: u32, r: f32, p: Vec2, a0: f32) -> Point {
    let a = i as f32 * (2.0 * PI / (n as f32));
    let x = (r * (1.0 + a.sin() * 0.3) * (a0 + a).cos()) as i32;
    let y = (r * (1.0 + a.cos() * 0.3) * (a0 + a).sin()) as i32;
    Point::new(p.x as i32 + x, p.y as i32 + y)
}

fn render_asteroid(
    canvas: &mut WindowCanvas,
    asteroid: &Asteroid,
    pos: &Pos,
    rot: &Rot,
) {
    let mut points = [Point::new(0, 0); ASTEROID_EDGES_RANGE.1 as usize * 2];
    for i in 0..asteroid.edges as usize {
        points[2*i] = circloid_point(i as u32, asteroid.edges, asteroid.radius, pos.0, rot.0);
        points[2*i+1] = circloid_point((i + 1) as u32, asteroid.edges, asteroid.radius, pos.0, rot.0);
    }
    canvas.draw_lines(&points[0 .. asteroid.edges as usize * 2]).unwrap();
}
