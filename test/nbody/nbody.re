/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Troestler Christophe
 * 
 * Revised BSD license
 * 
 * This is a specific instance of the Open Source Initiative (OSI) BSD license
 * template
 * http://www.opensource.org/licenses/bsd-license.php
 * 
 * Copyright 2008-2012 Isaac Gouy
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 *    Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 
 *    Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 
 *    Neither the name of "The Computer Language Benchmarks Game" nor the name of
 *    "The Computer Language Shootout Benchmarks" nor the name "bencher" nor the
 *    names of its contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

let pi = 3.141592653589793;
let solar_mass = 4. *. pi *. pi;
let days_per_year = 365.24;

type planet = {
  mutable x: float,
  mutable y: float,
  mutable z: float,
  mutable vx: float,
  mutable vy: float,
  mutable vz: float,
  mass: float,
};

let advance = (bodies, dt) => {
  let n = Array.length(bodies) - 1;
  for (i in 0 to Array.length(bodies) - 1) {
    let b = bodies[i];
    for (j in i + 1 to Array.length(bodies) - 1) {
      let b' = bodies[j];
      let dx = b.x -. b'.x
      and dy = b.y -. b'.y
      and dz = b.z -. b'.z;
      let dist2 = dx *. dx +. dy *. dy +. dz *. dz;
      let mag = dt /. (dist2 *. sqrt(dist2));

      b.vx = b.vx -. dx *. b'.mass *. mag;
      b.vy = b.vy -. dy *. b'.mass *. mag;
      b.vz = b.vz -. dz *. b'.mass *. mag;

      b'.vx = b'.vx +. dx *. b.mass *. mag;
      b'.vy = b'.vy +. dy *. b.mass *. mag;
      b'.vz = b'.vz +. dz *. b.mass *. mag;
    };
  };
  for (i in 0 to n) {
    let b = bodies[i];
    b.x = b.x +. dt *. b.vx;
    b.y = b.y +. dt *. b.vy;
    b.z = b.z +. dt *. b.vz;
  };
};

let energy = bodies => {
  let e = ref(0.);
  for (i in 0 to Array.length(bodies) - 1) {
    let b = bodies[i];
    e := e^ +. 0.5 *. b.mass *. (b.vx *. b.vx +. b.vy *. b.vy +. b.vz *. b.vz);
    for (j in i + 1 to Array.length(bodies) - 1) {
      let b' = bodies[j];
      let dx = b.x -. b'.x
      and dy = b.y -. b'.y
      and dz = b.z -. b'.z;
      let distance = sqrt(dx *. dx +. dy *. dy +. dz *. dz);
      e := e^ -. b.mass *. b'.mass /. distance;
    };
  };
  e^;
};

let offset_momentum = bodies => {
  let px = ref(0.)
  and py = ref(0.)
  and pz = ref(0.);
  for (i in 0 to Array.length(bodies) - 1) {
    px := px^ +. bodies[i].vx *. bodies[i].mass;
    py := py^ +. bodies[i].vy *. bodies[i].mass;
    pz := pz^ +. bodies[i].vz *. bodies[i].mass;
  };
  bodies[0].vx = -. px^ /. solar_mass;
  bodies[0].vy = -. py^ /. solar_mass;
  bodies[0].vz = -. pz^ /. solar_mass;
};

let jupiter = {
  x: 4.84143144246472090e+00,
  y: (-1.16032004402742839e+00),
  z: (-1.03622044471123109e-01),
  vx: 1.66007664274403694e-03 *. days_per_year,
  vy: 7.69901118419740425e-03 *. days_per_year,
  vz: (-6.90460016972063023e-05) *. days_per_year,
  mass: 9.54791938424326609e-04 *. solar_mass,
};

let saturn = {
  x: 8.34336671824457987e+00,
  y: 4.12479856412430479e+00,
  z: (-4.03523417114321381e-01),
  vx: (-2.76742510726862411e-03) *. days_per_year,
  vy: 4.99852801234917238e-03 *. days_per_year,
  vz: 2.30417297573763929e-05 *. days_per_year,
  mass: 2.85885980666130812e-04 *. solar_mass,
};

let uranus = {
  x: 1.28943695621391310e+01,
  y: (-1.51111514016986312e+01),
  z: (-2.23307578892655734e-01),
  vx: 2.96460137564761618e-03 *. days_per_year,
  vy: 2.37847173959480950e-03 *. days_per_year,
  vz: (-2.96589568540237556e-05) *. days_per_year,
  mass: 4.36624404335156298e-05 *. solar_mass,
};

let neptune = {
  x: 1.53796971148509165e+01,
  y: (-2.59193146099879641e+01),
  z: 1.79258772950371181e-01,
  vx: 2.68067772490389322e-03 *. days_per_year,
  vy: 1.62824170038242295e-03 *. days_per_year,
  vz: (-9.51592254519715870e-05) *. days_per_year,
  mass: 5.15138902046611451e-05 *. solar_mass,
};

let sun = {x: 0., y: 0., z: 0., vx: 0., vy: 0., vz: 0., mass: solar_mass};

let bodies = [|sun, jupiter, saturn, uranus, neptune|];

let () = {
  let n =
    try(int_of_string(Sys.argv[1])) {
    | _ =>
      Printf.printf(
        "No number of steps on command line, defaulting to 100\n",
      );
      100;
    };
  Printf.printf("n: %d\n", n);
  offset_momentum(bodies);
  Printf.printf("Initial energy: %.9f\n%!", energy(bodies));
  for (_i in 1 to n) {
    advance(bodies, 0.01);
  };
  Printf.printf("Final energy: %.9f\n%!", energy(bodies));
};
