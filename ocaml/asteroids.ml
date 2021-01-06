
module Log = struct
  type log_level =
    | INFO
    | WARN
    | ERROR

  let log_level = ref INFO

  let info sys msg =
    if !log_level >= INFO then
      Printf.printf "%s: %s\n" sys msg
end

module FPS = struct
  let file = open_out "fps.out"
  let t0 = ref 0.0
  let d0 = ref 0.0
  let init () = t0 := Unix.gettimeofday ()
  let current () =
    let t1 = Unix.gettimeofday () in
    let d  = t1 -. !t0 in
    d0 := d;
    t0 := t1;
    1.0 /. d

  let write frame =
    let fps = current () in
    Printf.fprintf file "%d,%d\n" frame (Float.to_int fps)
end

module Random = struct
  include Random

  let int_between   l h = l +  int   (h -  l)
  let float_between l h = l +. float (h -. l)
end

module V2F = struct
  type t = {x: float; y: float}
  let add l r =
    { x = l.x +. r.x
    ; y = l.y +. r.y
    }

  let rotate v angle =
    let s = sin angle
    and c = cos angle
    in
      { x = v.x *. c -. v.y *. s
      ; y = v.x *. s +. v.y *. c
      }

  let dist_sqr l r =
    let dx = r.x -. l.x in
    let dy = r.y -. l.y in
      dx *. dx +. dy *. dy

  let scale v s =
    { x = v.x *. s
    ; y = v.y *. s
    }

  let polar ~len ~rot =
    { x = len *. cos(rot)
    ; y = len *. sin(rot)
    }
end

module Constants = struct
  let mAX_ENTITY_COUNT = 1024
  let wORLD_HEIGHT = 1024.0
  let wORLD_WIDTH = 1480.0
  let pLAYER_MASS = 100000.0
  let pLAYER_RADIUS = 15.0
  let pLAYER_THRUST = 20000000.0
  let pLAYER_ROTATION_SPEED = Float.pi
  let pLAYER_FIRE_COOLDOWN = 0.3
  let pLAYER_HEIGHT = 25.0
  let pLAYER_WIDTH = 30.0
  let pLAYER_DY = pLAYER_HEIGHT /. 2.
  let pLAYER_DX = pLAYER_WIDTH /. 2.
  let aSTEROID_AMOUNT = 20
  let aSTEROID_DENSITY = 1.0
  let aSTEROID_SIZE_MIN = 10.0
  let aSTEROID_SIZE_MAX = 130.0
  let aSTEROID_EDGE_MIN = 8
  let aSTEROID_EDGE_MAX = 20
  let aSTEROID_VELOCITY_MIN = -100.0
  let aSTEROID_VELOCITY_MAX = 100.0
  let bULLET_LIFE_TIME = 3.0
  let bULLET_RADIUS = 1.0
  let bULLET_SPEED = 200.0
end

exception CannotCreateEntity

module Entity = struct
  type t = int
  let counter = ref 0
  let allocate () =
    let old = !counter in
    if old >= Constants.mAX_ENTITY_COUNT then
      raise CannotCreateEntity
    else
      counter := !counter + 1; old

end

module Components = struct
  let position, rotation, velocity, mass = 1, 2, 4, 8
  let radius, player, asteroid, bullet = 16, 32, 64, 128
  let alive = 256
end

let error_msg = ref ""

module Tag = struct
  type t = int

  let storage : t array = Array.make Constants.mAX_ENTITY_COUNT 0

  let has    e tag = (storage.(e) land tag) == tag
  let add    e tag = storage.(e) <- storage.(e) lor tag
  let remove e tag = storage.(e) <- storage.(e) land lnot tag
  let get    e     = storage.(e)

  let str e =
    if has e Components.asteroid then "A" else
    if has e Components.player   then "P" else
    if has e Components.bullet   then "." else
      "?"
end

module type COMPONENT = sig
  type t
  val  tag     : Tag.t
  val  default : t
end

module Component (Comp : COMPONENT) = struct
  let storage    = Array.make Constants.mAX_ENTITY_COUNT Comp.default
  let set    e c = storage.(e) <- c; Tag.add e Comp.tag
  let remove e   = Tag.remove e Comp.tag
  let get    e   = storage.(e) [@@inline always]
  let has    e   = Tag.has e Comp.tag
end

module MassSchema = struct
  type t       = float
  let  tag     = Components.mass
  let  default = 0.0
end

module RadiusSchema = struct
  type t       = float
  let  tag     = Components.radius
  let  default = 0.0
end

module RotationSchema = struct
  type t       = float
  let  tag     = Components.rotation
  let  default = 0.0
end

module PositionSchema = struct
  type t       = V2F.t
  let  tag     = Components.position
  let  default = V2F.({x = 0.0; y = 0.0})
end

module VelocitySchema = struct
  type t       = V2F.t
  let  tag     = Components.velocity
  let  default = V2F.({x = 0.0; y = 0.0})
end

module PlayerSchema = struct
  type t           = {thrust: bool; fire_cooldown: float}
  let  tag         = Components.player
  let  default : t = {thrust = false; fire_cooldown = 0.0}
end

module BulletSchema = struct
  type t       = {life_time: float}
  let  tag     = Components.bullet
  let  default = {life_time = 0.0}
end

module AsteroidSchema = struct
  type t       = {edges: int}
  let  tag     = Components.asteroid
  let  default = {edges = 0}
end

module Rotation = Component(RotationSchema)
module Radius   = Component(RadiusSchema)
module Mass     = Component(MassSchema)
module Position = Component(PositionSchema)
module Velocity = Component(VelocitySchema)

module Player = struct
  module Impl = Component(PlayerSchema)
  let self = ref (-1)
  let set    e c = Impl.set e c; self := e
  let has    e   = e == !self
  let remove e   = Impl.remove e; self := -1
  let get    e   = Impl.storage.(e)

  let spawn ~player ~pos ~vel ~rot ~mass ~radius =
    let e = Entity.allocate () in
    set          e player;
    Position.set e pos;
    Velocity.set e vel;
    Rotation.set e rot;
    Mass.set     e mass;
    Radius.set   e radius;
    Tag.add      e Components.alive

  let respawn ~e ~player ~pos ~vel ~rot =
    set          e player;
    Position.set e pos;
    Velocity.set e vel;
    Rotation.set e rot

  let direction () =
    let e     = !self in
    let v     = V2F.({x = 1.0; y = 0.0}) in
    let angle = Rotation.get e in
    V2F.rotate v angle
end

module Bullet = struct
  include Component(BulletSchema)

  let spawn ~bullet ~pos ~vel ~radius =
    let e = Entity.allocate () in
    set          e bullet;
    Position.set e pos;
    Velocity.set e vel;
    Radius.set   e radius;
    Tag.add      e Components.alive

  let destroy e =
    remove          e;
    Position.remove e;
    Velocity.remove e;
    Rotation.remove e;
    Tag.remove      e Components.alive

  let lifetime ~e ~dt =
    if has e then begin
      let time = (get e).life_time in
      let new_time = time -. dt in
      if new_time < 0.0 then
        remove e
      else
        set e {life_time = new_time}
      end
end

let collision e ~prev_collided : int option =
  let e2start = 1 + if prev_collided >= 0 then prev_collided else e in
  if Tag.has e (Components.position lor Components.radius) then begin
    let p1 = Position.get e in
    let r1 = Radius.get   e in
    let maxE = !Entity.counter in
    let rec loop e2 : int option =
      if e2 >= maxE then None else begin
        if Tag.has e2 Components.alive then begin
          let p2 = Position.get e2 in
          let r2 = Radius.get   e2 in
          let r  = r1 +. r2 in
          let dist = V2F.dist_sqr p1 p2 in
          (* Log.info "collision" @@ Int.to_string e ^ " x " ^ Int.to_string e2 ^ ": dist = " ^ Float.to_string dist ^ ", r1 = " ^ Float.to_string r1 ^ ", r2 = " ^ Float.to_string r2; *)
          if dist < r *. r then
            Some e2
          else
            loop (e2 + 1)
        end else
          loop (e2 + 1)
      end
    in
      loop e2start
  end else
    None

let movement e ~dt =
  if Tag.has e (Components.position lor Components.velocity) then begin
    let p = Position.get e in
    let v = Velocity.get e in
    let p1 = V2F.add p (V2F.scale v dt) in
    Position.set e p1
  end


module Asteroid = struct
  include Component(AsteroidSchema)

  let spawn ~asteroid ~pos ~vel ~rot ~mass ~radius =
    let e = Entity.allocate () in
    set          e asteroid;
    Position.set e pos;
    Velocity.set e vel;
    Rotation.set e rot;
    Mass.set     e mass;
    Radius.set   e radius;
    Tag.add      e Components.alive

  let destroy e =
    remove          e;
    Position.remove e;
    Velocity.remove e;
    Rotation.remove e;
    Mass.remove     e;
    Radius.remove   e;
    Tag.remove      e Components.alive
end

module Input = struct
  type t =
    { left   : bool
    ; right  : bool
    ; thrust : bool
    ; fire   : bool
    }
end

module Movement = struct
  let apply ~e ~dt =
    if Tag.has e (Components.position lor Components.velocity) then begin
      let pos = Position.get e in
      let vel = Velocity.get e in
      Position.set e @@ V2F.(add pos (scale vel dt))
    end;
    if Position.has e then begin
      let open V2F in
      let open Constants in
      let pos = Position.get e in
      Position.set e @@
        { x =
          if pos.x < 0.0          then pos.x +. wORLD_WIDTH else
          if pos.x >= wORLD_WIDTH then pos.x -. wORLD_WIDTH else
            pos.x
        ; y =
          if pos.y < 0.0           then pos.y +. wORLD_HEIGHT else
          if pos.y >= wORLD_HEIGHT then pos.y -. wORLD_HEIGHT else
            pos.y
        }
    end
end

module World = struct
  let rec prepare () =
    Player.spawn
      ~player: PlayerSchema.({thrust = false; fire_cooldown = 0.0})
      ~pos:    V2F         .({x = Constants.wORLD_WIDTH /. 2.; y = Constants.wORLD_HEIGHT /. 2.})
      ~vel:    V2F         .({x = 0.; y = 0.})
      ~rot:    0.
      ~radius: Constants.pLAYER_RADIUS
      ~mass:   Constants.pLAYER_MASS;
    spawn_asteroids Constants.aSTEROID_AMOUNT

  and spawn_asteroids count =
    for _ = 0 to count - 1 do
      (* Log.info "world" "spawning an asteroid"; *)
      let radius = Constants.(Random.float_between aSTEROID_SIZE_MIN aSTEROID_SIZE_MAX) in
      Asteroid.spawn
        ~asteroid: AsteroidSchema.(Constants.({edges = Random.int_between aSTEROID_EDGE_MIN aSTEROID_EDGE_MAX}))
        ~pos:      V2F.           (Constants.({x = Random.float wORLD_WIDTH; y = Random.float wORLD_HEIGHT}))
        ~vel:      (add_random_asteroid_velocity V2F.({x = 0.; y = 0.}))
        ~rot:      (Random.float Float.pi)
        ~mass:     Constants.(aSTEROID_DENSITY *. Float.pi *. radius *. radius)
        ~radius
    done

  and add_random_asteroid_velocity v =
    { x = v.x +. Constants.(Random.float_between aSTEROID_VELOCITY_MIN aSTEROID_VELOCITY_MAX)
    ; y = v.y +. Constants.(Random.float_between aSTEROID_VELOCITY_MIN aSTEROID_VELOCITY_MAX)
    }

  let update_player ~dt =
    let e = !Player.self in
    let player = Player.get e in
    if PlayerSchema.(player.thrust) then begin
      let rot  = Rotation.get e in
      let mass = Mass.get     e in
      let acc  = dt *. Constants.pLAYER_THRUST /. mass in
      let vel  = Velocity.get e in
      let new_vel = V2F.add vel (V2F.polar ~len: acc ~rot) in
      Velocity.set e new_vel
    end;
    (* Log.info "cooldown" @@ Float.to_string player.fire_cooldown; *)
    if PlayerSchema.(player.fire_cooldown > 0.) then begin
      let new_cooldown = Float.max 0.0 (PlayerSchema.(player.fire_cooldown) -. dt) in
      Player.set e { player with fire_cooldown = new_cooldown}
    end


  let apply_input ~input ~dt =
    let e      = !Player.self in
    let player = Player.get e in
    let rot    = Rotation.get e in
    let dRot = Constants.(
      if Input.(input.left) && not Input.(input.right) then
        -. pLAYER_ROTATION_SPEED *. dt
      else if not Input.(input.left) && Input.(input.right) then
           pLAYER_ROTATION_SPEED *. dt
      else
           0.0
    )
    in
    Rotation.set e (rot +. dRot);

    let thrust = Input.(input.thrust) in

    if Input.(input.fire) && PlayerSchema.(player.fire_cooldown) <= 0.0 then begin
      let pos = Position.get e in
      let vel = Velocity.get e in
      let rot = Rotation.get e in
      let bVel = V2F.add vel (V2F.polar ~len: Constants.bULLET_SPEED ~rot) in
      Bullet.spawn
        ~bullet: BulletSchema.({life_time = Constants.bULLET_LIFE_TIME})
        ~vel: bVel
        ~pos: pos
        ~radius: Constants.bULLET_RADIUS;
      Player.set e
        { thrust        = thrust
        ; fire_cooldown = Constants.pLAYER_FIRE_COOLDOWN
        }
    end else
      Player.set e
        { player with
            thrust = thrust
        }

  let respawn_player () =
    Player.respawn
      ~e:      !Player.self
      ~player: PlayerSchema.({thrust = false; fire_cooldown = Constants.pLAYER_FIRE_COOLDOWN})
      ~pos:    V2F.(Constants.({x = wORLD_WIDTH /. 2.0; y = wORLD_HEIGHT /. 2.0}))
      ~vel:    V2F.({x = 0.; y = 0.})
      ~rot:    0.0

  let rec break_asteroid ~bullet ~asteroid =
    Bullet.destroy bullet;
    spawn_cracks ~asteroid;
    Asteroid.destroy asteroid

  and spawn_cracks ~asteroid =
    let radius : float = 0.5 *. Radius.get asteroid in
    if radius >= Constants.aSTEROID_SIZE_MIN then begin
      let pos : V2F.t = Position.get asteroid in
      let vel : V2F.t = Velocity.get asteroid in
      let mass : float = Constants.(aSTEROID_DENSITY *. Float.pi *. radius *. radius) in
      for _ = 1 to 2 do
        Asteroid.spawn
          ~asteroid: AsteroidSchema.(Constants.({edges = Random.int_between aSTEROID_EDGE_MIN aSTEROID_EDGE_MAX}))
          ~pos
          ~vel:      (add_random_asteroid_velocity vel)
          ~rot:      (Random.float Float.pi)
          ~mass:      mass
          ~radius:    radius
      done
    end

  let spawn_player ~player ~pos ~vel ~rot ~mass =
    Player.spawn ~player ~pos ~vel ~rot ~mass
      ~radius: Constants.pLAYER_RADIUS

  let step ~dt ~input =
    update_player ~dt;
    apply_input   ~dt ~input;
    for e = 0 to !Entity.counter - 1 do
      if Tag.has e Components.alive then begin
        Bullet.lifetime ~e ~dt;
        Movement.apply ~e ~dt;
        let rec loop ~oldColl =
          match collision e ~prev_collided: oldColl with
          | None -> ()
          | Some ecoll -> begin
            (* Log.info "collision" (Int.to_string e ^ "/" ^ Tag.str e ^ " with " ^ Int.to_string ecoll ^ "/" ^ Tag.str ecoll); *)
            if Player.has   e && Asteroid.has ecoll then respawn_player ();
            if Asteroid.has e && Player.has   ecoll then respawn_player ();
            if Bullet.has   e && Asteroid.has ecoll then break_asteroid ~bullet: e ~asteroid: ecoll;
            if Asteroid.has e && Bullet.has   ecoll then break_asteroid ~bullet: ecoll ~asteroid: e;
            loop ~oldColl: ecoll
          end
        in
          loop ~oldColl: (-1)
      end
    done
end

module Render = struct
  let rec render_player ~renderer ~player ~pos ~rot =
    Constants.(
      render_line ~renderer ~pos ~rot ~start: V2F.({x =   pLAYER_DX; y =   0.0})       ~finish: V2F.({x = -.pLAYER_DX; y = -.pLAYER_DY});
      render_line ~renderer ~pos ~rot ~start: V2F.({x =   pLAYER_DX; y =   0.0})       ~finish: V2F.({x = -.pLAYER_DX; y =   pLAYER_DY});
      render_line ~renderer ~pos ~rot ~start: V2F.({x = -.pLAYER_DX; y = -.pLAYER_DY}) ~finish: V2F.({x = -.pLAYER_DX; y =   pLAYER_DY});
      if PlayerSchema.(player.thrust) then begin
        render_line ~renderer ~pos ~rot ~start: V2F.({x = -.pLAYER_DX; y = -.0.5 *. pLAYER_DY}) ~finish: V2F.({x = -.pLAYER_DX-.10.0; y = 0.0});
        render_line ~renderer ~pos ~rot ~start: V2F.({x = -.pLAYER_DX; y =   0.5 *. pLAYER_DY}) ~finish: V2F.({x = -.pLAYER_DX-.10.0; y = 0.0});
      end
    )

  and render_line ~renderer ~pos ~rot ~start ~finish =
    V2F.(
      let s = rotate start  rot
      and f = rotate finish rot
      in Sdlrender.draw_line renderer
        ( ( Float.to_int @@ pos.x +. s.x
          , Float.to_int @@ pos.y +. s.y
          )
        , ( Float.to_int @@ pos.x +. f.x
          , Float.to_int @@ pos.y +. f.y
          )
        )
    )

  let render_bullet ~renderer ~pos =
    V2F.(
      Sdlrender.draw_point renderer
        ( Float.to_int @@ pos.x
        , Float.to_int @@ pos.y
        )
    )

  let rec render_asteroid ~renderer ~asteroid ~pos ~rot ~radius =
    AsteroidSchema.(
      render_circloid ~renderer ~edges: asteroid.edges ~pos ~rot ~radius
    )

  and render_circloid ~renderer ~edges ~pos ~rot ~radius =
    let points = Array.init (edges + 1) (circloid_point ~radius ~edges) in
    render_lines ~renderer ~pos ~rot ~points

  and circloid_point ~radius ~edges i =
    let angle = Float.of_int i *. (2. *. Float.pi /. Float.of_int edges) in
    let c = cos angle
    and s = sin angle
    in
      V2F.(
        { x = radius *. (1. +. s *. 0.3) *. c
        ; y = radius *. (1. +. c *. 0.3) *. s
        }
      )

  and render_lines ~renderer ~pos ~rot ~points =
    V2F.(
      let move_point pt =
        let {x; y} = add pos @@ rotate pt rot in
        (Float.to_int x, Float.to_int y)
      in
        Sdlrender.draw_lines renderer
        @@ Array.map move_point points
    )

  let render_entity ~renderer ~e =
    if Tag.has e (Components.position lor Components.rotation lor Components.player) then begin
      (* Log.info "render" ("player #" ^ Int.to_string e ^ " = " ^ Int.to_string (Tag.get e)); *)
      render_player
        ~renderer
        ~player: (Player.get   e)
        ~pos:    (Position.get e)
        ~rot:    (Rotation.get e)
    end else if Tag.has e (Components.position lor Components.rotation lor Components.asteroid lor Components.radius) then begin
      (* Log.info "render" ("asteroid #" ^ Int.to_string e); *)
      render_asteroid
        ~renderer
        ~asteroid: (Asteroid.get e)
        ~pos:      (Position.get e)
        ~rot:      (Rotation.get e)
        ~radius:   (Radius.get   e)
    end else if Tag.has e (Components.position lor Components.bullet) then begin
      (* Log.info "render" ("bullet #" ^ Int.to_string e); *)
      render_bullet
        ~renderer
        ~pos:    (Position.get e)
    end
end

module Main = struct
  include Sdlevent
  include Sdlkeycode
  include Input

  let rec process_event input : Input.t option =
    match poll_event () with
    | None -> Some input
    | Some event -> begin
      match event with
      | Quit    _                      -> None
      | KeyDown key when is key Escape -> None
      | KeyDown key when is key Up     -> Some { input with thrust = true }
      | KeyDown key when is key Left   -> Some { input with left   = true }
      | KeyDown key when is key Right  -> Some { input with right  = true }
      | KeyDown key when is key Space  -> Some { input with fire   = true }
      | KeyUp   key when is key Up     -> Some { input with thrust = false }
      | KeyUp   key when is key Left   -> Some { input with left   = false }
      | KeyUp   key when is key Right  -> Some { input with right  = false }
      | KeyUp   key when is key Space  -> Some { input with fire   = false }
      | _                              -> Some input
    end

  and is key code = key.keycode = code

  let start () =
    Constants.(
      Sdl.init [`EVERYTHING];
      let width, height = (Float.to_int wORLD_WIDTH, Float.to_int wORLD_HEIGHT) in
      let window, renderer = Sdlrender.create_window_and_renderer ~width ~height ~flags:[] in
      World.prepare ();
      FPS.init ();
      (* Log.info "mainloop" "start"; *)
      let rec loop ~frame ~input ~lastTick =
        (* Log.info "main loop" ("frame #" ^ Int.to_string frame); *)
        match process_event input with
        | None -> begin
          Sdlwindow.destroy window;
          Sdl.quit ()
        end

        | Some input ->
          let currentTick = Sdltimer.get_ticks () in
          let dt = Float.of_int (currentTick - lastTick) /. 1000.0 in
          World.step ~dt ~input;
          Sdlrender.set_draw_color3 renderer ~r:0 ~g:0 ~b:0 ~a:255;
          Sdlrender.clear renderer;
          Sdlrender.set_draw_color3 renderer ~r:255 ~g:255 ~b:255 ~a:255;
          for e = 0 to !Entity.counter - 1 do
            Render.render_entity ~renderer ~e
          done;
          Sdlrender.render_present renderer;
          FPS.write frame;
          loop ~input ~lastTick: currentTick ~frame: (frame + 1)

      in
        loop
          ~input:    Input.({thrust = false; left = false; right = false; fire = false})
          ~lastTick: (Sdltimer.get_ticks ())
          ~frame:    0
    )
  end

let () = Main.start ()
