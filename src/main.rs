#![allow(dead_code, incomplete_features)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(variant_count)]

use std::collections::VecDeque;

use derive_more::IsVariant;
use generic_array::ArrayLength;
use generic_array::GenericArray;

trait PrimInt:
    num::PrimInt + std::ops::BitAndAssign + std::ops::BitOrAssign + std::ops::BitXorAssign
{
}

impl<I: num::PrimInt + std::ops::BitAndAssign + std::ops::BitOrAssign + std::ops::BitXorAssign>
    PrimInt for I
{
}

#[derive(Debug, Clone, Copy)]
enum Side {
    North = 3,
    South = 2,
    East = 1,
    West = 0,
}

impl Side {
    fn opposite(&self) -> Side {
        use Side::*;
        match &self {
            North => South,
            South => North,
            East => West,
            West => East,
        }
    }
    fn all() -> &'static [Side] {
        use Side::*;
        const SIDES: [Side; core::mem::variant_count::<Side>()] = [North, South, East, West];
        &SIDES
    }
    fn as_int<I: PrimInt>(self) -> I {
        I::from(self).unwrap()
    }
}

impl num::ToPrimitive for Side {
    fn to_i64(&self) -> Option<i64> {
        Some(*self as i64)
    }

    fn to_u64(&self) -> Option<u64> {
        Some(*self as u64)
    }
}

#[derive(Clone, Copy)]
struct TileConnections<I: PrimInt = u8>(I);

impl<I: PrimInt> TileConnections<I> {
    fn new(n: bool, s: bool, e: bool, w: bool) -> Self {
        let mut state = I::zero();
        state |= I::from(n as u8).unwrap() << Side::North.as_int(); // 8
        state |= I::from(s as u8).unwrap() << Side::South.as_int(); // 4
        state |= I::from(e as u8).unwrap() << Side::East.as_int(); // 2
        state |= I::from(w as u8).unwrap() << Side::West.as_int(); // 1
        TileConnections(state)
    }

    fn from_number(value: I) -> Self {
        Self(value & I::from(0b1111).unwrap())
    }

    fn all() -> Self {
        Self(I::from(0b1111).unwrap())
    }

    fn none() -> Self {
        Self(I::from(0b0000).unwrap())
    }

    fn set(&mut self, side: Side, value: bool) -> () {
        self.0 &= !(I::one() << side.as_int());
        self.0 |= I::from(value as u8).unwrap() << side.as_int();
    }

    fn connects_to(&self, side: Side) -> bool {
        self.0 & (I::one() << side.as_int()) != I::zero()
    }

    fn as_int(self) -> I {
        self.0
    }
}

impl<I: PrimInt> std::ops::BitAnd for TileConnections<I> {
    type Output = TileConnections<I>;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl<I: PrimInt> std::ops::BitOr for TileConnections<I> {
    type Output = TileConnections<I>;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl<I: PrimInt> std::ops::Not for TileConnections<I> {
    type Output = TileConnections<I>;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl<I: PrimInt> std::fmt::Debug for TileConnections<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Side::*;
        write!(
            f,
            "TileConnections(N={}, S={}, E={}, W={})",
            self.connects_to(North) as u8,
            self.connects_to(South) as u8,
            self.connects_to(East) as u8,
            self.connects_to(West) as u8
        )
    }
}

#[derive(Debug, Clone, IsVariant)]
enum TileState {
    Collapsed(TileConnections),
    NotCollapsed { scheduled: bool },
}

impl Default for TileState {
    fn default() -> Self {
        TileState::NotCollapsed { scheduled: false }
    }
}

impl TileState {
    fn repr(&self) -> &'static str {
        match &self {
            TileState::NotCollapsed { .. } => "·",
            TileState::Collapsed(connections) => TILES[connections.as_int() as usize],
        }
    }
}

#[derive(Debug, Clone)]
struct Position {
    row: usize,
    col: usize,
}

#[derive(Debug, Clone)]
struct Tile {
    state: TileState,
    position: Position,
}

#[derive(Debug, Clone, Default)]
struct Grid<W: ArrayLength<TileState>, H: ArrayLength<GenericArray<TileState, W>>> {
    cells: GenericArray<GenericArray<TileState, W>, H>,
    collapsing_queue: VecDeque<Tile>,
}

impl<W: ArrayLength<TileState>, H: ArrayLength<GenericArray<TileState, W>>> Grid<W, H> {
    fn neighbor(&self, position: &Position, side: Side) -> Option<Tile> {
        use Side::*;
        let row_num = position.row.checked_add_signed(match side {
            North => -1,
            South => 1,
            _ => 0,
        })?;
        let col_num = position.col.checked_add_signed(match side {
            East => -1,
            West => 1,
            _ => 0,
        })?;
        let neighbor_state = self.cells.get(row_num)?.get(col_num)?;
        let neighbor_pos = Position { row: row_num, col: col_num };
        Some(Tile {
            state: neighbor_state.clone(),
            position: neighbor_pos,
        })
    }

    fn collapse_step<R: rand::Rng>(&mut self, rng: &mut R) {
        for _ in 0..self.collapsing_queue.len() {
            let curr_tile = self.collapsing_queue.pop_front().unwrap();
            if curr_tile.state.is_collapsed() {
                continue;
            }
            let (cant_connect, must_connect) = self.check_neighbors_states(&curr_tile.position);
            let random_connections = TileConnections::from_number(rng.gen_range(0..16) & 0b1111);
            let connections = (random_connections & !cant_connect) | must_connect;

            self.cells[curr_tile.position.row][curr_tile.position.col] =
                TileState::Collapsed(connections);
        }
    }

    fn collapse_all<R: rand::Rng>(&mut self, rng: &mut R) {
        while !self.collapsing_queue.is_empty() {
            self.collapse_step(rng);
        }
    }

    fn check_neighbors_states(
        &mut self,
        cell_pos: &Position,
    ) -> (TileConnections<u8>, TileConnections<u8>) {
        use TileState::*;

        let mut cant_connect = TileConnections::none();
        let mut must_connect = TileConnections::none();

        for side in Side::all() {
            match self.neighbor(cell_pos, *side) {
                Some(Tile { state: Collapsed(neighbor), .. }) => {
                    let n_status = neighbor.connects_to(side.opposite());
                    must_connect.set(*side, n_status);
                    cant_connect.set(*side, !n_status);
                }
                Some(Tile { state: NotCollapsed { scheduled }, position }) => {
                    if !scheduled {
                        let new_tile = Tile { state: NotCollapsed { scheduled: true }, position };
                        *self
                            .cells
                            .get_mut(new_tile.position.row)
                            .unwrap()
                            .get_mut(new_tile.position.col)
                            .unwrap() = new_tile.state.clone();
                        self.collapsing_queue.push_back(new_tile);
                    }
                }
                None => cant_connect.set(*side, true),
            }
        }

        (cant_connect, must_connect)
    }

    fn schedule_cell_collapse(&mut self, row: usize, col: usize) -> Option<bool> {
        let cell = self.cells.get_mut(row)?.get_mut(col)?;
        if let TileState::NotCollapsed { scheduled: false } = cell {
            *cell = TileState::NotCollapsed { scheduled: true };
            self.collapsing_queue.push_back(Tile {
                state: cell.clone(),
                position: Position { row, col },
            });
            return Some(true);
        }
        Some(false)
    }

    fn seed_grid<R: rand::Rng>(&mut self, rng: &mut R, count: usize) {
        for _ in 0..count {
            loop {
                let row = rng.gen_range(0..(H::to_usize()));
                let col = rng.gen_range(0..(W::to_usize()));
                if self.schedule_cell_collapse(row, col).unwrap_or(false) {
                    break;
                }
            }
        }
    }
}

impl<W: ArrayLength<TileState>, H: ArrayLength<GenericArray<TileState, W>>> std::fmt::Display
    for Grid<W, H>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in self.cells.iter() {
            for cell in row.iter() {
                write!(f, "{}", cell.repr())?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

const TILES: [&'static str; 16] = [
    " ", "╺", "╸", "━", "╻", "┏", "┓", "┳", "╹", "┗", "┛", "┻", "┃", "┣", "┫", "╋",
];

fn main() -> std::io::Result<()> {
    let mut grid = Grid::<typenum::U80, typenum::U30>::default();
    let mut rng = rand::thread_rng();
    grid.seed_grid(&mut rng, 10);

    use clap::Parser;
    match Cli::parse().mode {
        Mode::PrintOnce => {
            grid.collapse_all(&mut rng);
            print!("{}", grid);
        }
        Mode::PrintStep => {
            cli_iter_steps(&mut grid, &mut rng);
        }
        Mode::TimeIt => {
            use num_format::Locale;
            use num_format::ToFormattedString;
            let start = std::time::Instant::now();
            grid.collapse_all(&mut rng);
            println!(
                "Ran in {} ns",
                (std::time::Instant::now() - start).as_nanos().to_formatted_string(&Locale::en)
            );
        }
    }

    Ok(())
}

#[derive(Debug, Clone, clap::ValueEnum)]
enum Mode {
    PrintOnce,
    PrintStep,
    TimeIt,
}

#[derive(clap::Parser)]
struct Cli {
    #[clap(value_enum)]
    mode: Mode,
}

fn cli_iter_steps<
    W: ArrayLength<TileState>,
    H: ArrayLength<GenericArray<TileState, W>>,
    R: rand::Rng,
>(
    grid: &mut Grid<W, H>,
    rng: &mut R,
) {
    use std::io::Read;
    let mut stdin = std::io::stdin();

    while !grid.collapsing_queue.is_empty() {
        grid.collapse_step(rng);
        print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
        print!("{}", grid);
        println!("press enter for next step");
        let _ = stdin.read(&mut [0u8]).unwrap();
    }
}
