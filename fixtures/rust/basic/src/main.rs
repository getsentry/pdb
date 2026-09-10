// src/main.rs

use std::collections::HashMap;

#[derive(Debug)]
struct Point {
    x: i32,
    y: i32,
    label: String,
}

#[derive(Debug)]
enum Shape {
    Point(Point),
    Circle { center: Point, radius: f64 },
    Polygon(Vec<Point>),
}

impl Shape {
    fn area(&self) -> f64 {
        match self {
            Shape::Point(_) => 0.0,
            Shape::Circle { radius, .. } => std::f64::consts::PI * radius * radius,
            Shape::Polygon(points) => {
                let n = points.len();
                if n < 3 {
                    return 0.0;
                }
                let mut sum = 0.0;
                for i in 0..n {
                    let a = &points[i];
                    let b = &points[(i + 1) % n];
                    sum += (a.x * b.y - b.x * a.y) as f64;
                }
                sum.abs() / 2.0
            }
        }
    }
}

trait Describe {
    fn describe(&self) -> String;
}

impl Describe for Shape {
    fn describe(&self) -> String {
        match self {
            Shape::Point(p) => format!("Point at ({}, {})", p.x, p.y),
            Shape::Circle { center, radius } => {
                format!("Circle at ({}, {}) r={}", center.x, center.y, radius)
            }
            Shape::Polygon(points) => format!("Polygon with {} vertices", points.len()),
        }
    }
}

fn build_shapes() -> Vec<Shape> {
    let mut shapes = Vec::new();

    shapes.push(Shape::Point(Point { x: 0, y: 0, label: "origin".into() }));
    shapes.push(Shape::Circle {
        center: Point { x: 10, y: 10, label: "c".into() },
        radius: 5.0,
    });
    shapes.push(Shape::Polygon(vec![
        Point { x: 0, y: 0, label: "a".into() },
        Point { x: 4, y: 0, label: "b".into() },
        Point { x: 4, y: 4, label: "c".into() },
        Point { x: 0, y: 4, label: "d".into() },
    ]));

    shapes
}

fn count_by_kind(shapes: &[Shape]) -> HashMap<&'static str, usize> {
    let mut counts = HashMap::new();
    for s in shapes {
        let key = match s {
            Shape::Point(_) => "point",
            Shape::Circle { .. } => "circle",
            Shape::Polygon(_) => "polygon",
        };
        *counts.entry(key).or_insert(0) += 1;
    }
    counts
}

fn main() {
    let shapes = build_shapes();

    for s in &shapes {
        println!("{} (area = {:.2})", s.describe(), s.area());
    }

    let counts = count_by_kind(&shapes);
    println!("counts: {:?}", counts);
}