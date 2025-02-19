using Viznet
using Viznet: inner_most_containers, put_edge!, put_node!, empty_cache!, nedge, nnode, ncanvas, CACHE
using Test
using Compose

@testset "basic" begin
    empty_cache!()
    c = compose(context(), compose(context(), line()))
    n = compose(context(), compose(context(), circle()))
    ics = []
    inner_most_containers(x->!isempty(x.form_children) && push!(ics, x), c)
    @test length(ics) == 1
    ic = ics[]
    @test first(ic.form_children) isa Compose.Form{<:Compose.LinePrimitive}
    put_edge!(c, (0.3, 0.4), (2.3, 1.4))
    put_edge!(c, (0.3, 0.5), (2.3, 1.4))
    put_node!(n, (0.3, 0.5))
    @test nnode() == 1
    @test nedge() == 2
    empty_cache!()
    @test nnode() == 0
    @test nedge() == 0
end


@testset "flush edges" begin
    empty_cache!()
    c = compose(context(), compose(context(), line()))
    c >> ((0.3, 0.4), (2.3, 1.4))
    c >> ((0.3, 0.8), (2.3, 1.9))
    c0 = bondstyle(:default)
    c0 >> ((0.3, 0.8), (2.3, 2.9))
    lst = flush!(CACHE.edge)
    @test length(lst) == 2
end

@testset "flush nodes" begin
    empty_cache!()
    c = compose(context(), compose(context(), circle(0.2, 0.2, 2.0)))
    c >> (0.3, 0.4)
    c >> (0.3, 0.8)
    c0 = nodestyle(:default)
    c0 >> (0.3, 0.8)
    lst = flush!(CACHE.node)
    @test length(lst) == 2
end

@testset "system test" begin
    empty_cache!()
    nb = compose(context(), stroke("red"), circle(0.0, 0.0, 0.02))
    eb = compose(context(), stroke("green"), line())

    x = nb >> (0.2, 0.3)
    y = nb >> (0.6, 0.6)
    eb >> (x, y)
    @test flush!() isa Context
end

@testset "similar shape" begin
    for p2 in [
        polygon([(0.1, 0.2), (0.2, 0.3), (0.3,0.3)]),
        circle(0.1, 0.2, 0.3),
        rectangle(0.1,0.1, 0.2, 0.2),
        line(),
        arc(0.1, 0.1, 0.2, π, π/2),
        curve((0.0, 0.1), (0.2, 0.2), (0.3, 0.4), (0.5, 0.0)),
        ]
        nodes = Viznet.similar(p2, [(0.1,0.1), (0.2,0.2), (0.3,0.3), (0.4, 0.4)])
        @test nodes.primitives |> length == 4
    end
    for l2 in [
        line(),
        arc(0.1, 0.1, 0.2, π, π/2),
        curve((0.0, 0.1), (0.2, 0.2), (0.3, 0.4), (0.5, 0.0)),
        ]
        lines = Viznet.similar(l2, [((0.1,0.1), (0.2,0.2)), ((0.3,0.3), (0.4, 0.4))])
        @test lines.primitives |> length == 2
    end
end

@testset "inner canvas" begin
    c = compose(context(), compose(context(), line()))
    n = compose(context(), compose(context(), circle()))
    canvas() do 
        c >> ((0, 1), (2, 3))
        n >> (0, 1)
        inner = canvas() do
            c >> ((2, 3), (4, 5))
            c >> ((2, 3), (4, 5))
            @test nnode() == 0
            @test nedge() == 2
        end

        @test nnode() == 1
        @test nedge() == 1
        @test ncanvas() == 0

        inner >> (0, 1, 2, 3)

        @test nnode() == 1
        @test nedge() == 1
        @test ncanvas() == 1
    end
end