void build(Solution &s)
{
    auto &t = s.addTarget<Executable>("asteroids");
    t += cpp20;

    t += "org.sw.demo.valve.sdl.mixer"_dep;
    t += "org.sw.demo.valve.sdl.main"_dep;
    t += "org.sw.demo.skypjack.entt"_dep;
}
