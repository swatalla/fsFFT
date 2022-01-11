namespace SignalFS


// The MIT License

// Copyright (c) 2021 Sebastian Atalla

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

open Functions
open System
open System.Numerics

// TODO - look into using the Vector<'T> class instead of arrays
// TODO - looks like I actually need to make a Vector<Complex>...
//  or, I split the arrays up into a real array and an imag array - SOA vs AOS

// TODO - this should theoretically be more efficient if i use separate arrays for
// real and imaginary parts of the signal, rather than a Complex array.
// a Complex array is an array of structs; struct of arrays (SOA) is more efficient
// Possibly define a signal type, that consists of 2 arrays, real and imag?
// ##############################
// type Signal =               ##
//     val Real: double[]      ##   <- Like this
//     val Imaginary: double[] ##
// ##############################


/// <summary> Handle for specifying direction of FFT algorithm. </summary>
type internal Compute = | Forward | Reverse

[<AutoOpen>]
/// <summary> Contains custom operators. </summary>
module private Operators =
          
    /// <summary>
    /// Alias for System.Math.PI
    /// </summary>
    let pi = Math.PI

    /// <summary>
    /// Infix operator for performing operations on both real and imaginary components of a Complex number.
    /// </summary>
    /// <param name="c"></param>
    /// <param name="x"></param>
    let inline (.+) (c: Complex) (x: float) = Complex(c.Real+x, c.Imaginary+x)
    let inline (.-) (c: Complex) (x: float) = Complex(c.Real-x, c.Imaginary-x)
    let inline (./) (c: Complex) (x: float) = Complex(c.Real/x, c.Imaginary/x)
    let inline (.*) (c: Complex) (x: float) = Complex(c.Real*x, c.Imaginary*x)

    /// <summary>
    /// Infix operator for appending arrays.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    let inline (@) (x: ^a[]) (y: ^a[]) = Array.append x y
    // this has O(m) complexity; make sure y is the shorter array.

    /// <summary>
    /// Base 2 logarithm.
    /// </summary>
    /// <param name="x"></param>
    let inline log2 (x: ^a) = log (double x) / log 2.0

    /// <summary>
    /// Gets the coefficient for the respective FFT algorithm.
    /// </summary>
    let coefficient = function | Compute.Forward -> -1.0 | Compute.Reverse -> 1.0

    /// <summary>
    /// Active pattern for determining if vector length is a power of 2.
    /// </summary>
    /// <param name="n"></param>
    let (|Radix2|_|) n = if n &&& n - 1 = 0 then Some Radix2 else None


/// <summary> Contains functions for bit manipulation. </summary>
module private Bit =

    /// <summary>
    /// Removes leading-zero bytes from integer byte-array.
    /// </summary>
    /// <param name="x"></param>
    let trimBytes x = 
        match x with
        | 0 -> [|0uy|]
        | _ -> BitConverter.GetBytes(x) 
               |> fun b ->  b.[0..Array.FindLastIndex(b, fun k -> k <> 0uy)]

    /// <summary>
    /// Reverses the bytes of an integer byte-array.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="bits"></param>
    let reverse x bits =
        let rec reverse' x bits y =
            match bits with
            | 0 -> y
            | _ -> ((y <<< 1) ||| (x &&& 1))
                   |> reverse' (x >>> 1) (bits - 1) 
        reverse' x bits 0 

    /// <summary>
    /// Gets the number of bits of which an integer is comprised.
    /// </summary>
    /// <param name="num"></param>
    let length (num: int) =
        let xb = num |> trimBytes
        let msb = xb.[xb.Length - 1]
        let rec length' a = function
            | 0uy  -> a
            | msb' -> (a + 1, msb' >>> 1) ||> length'
        ((xb.Length - 1) * 8, msb) ||> length'


/// <summary> Contains functions for transforming a signal. </summary>
module private Fourier =

    /// <summary>
    /// Radix-2 decimation-in-time FFT algorithm.
    /// </summary>
    /// <param name="vector"></param>
    /// <param name="parity"></param>
    let decimate (parity: Compute) (vector: Complex[]) =
        let z = vector.Length
        let depth = z |> log2 |> floor |> int
        if (1 <<< depth) <> z then failwith "Vector length is not a power of 2"

        // Complex roots of unity; "twiddle factors"
        let unity: Complex[] =
            let xpn = ((coefficient parity) * 2.0) * pi / double z
            Array.init<Complex> (z/2) (fun i ->
                Complex.FromPolarCoordinates(1.0, (float i) * xpn))

        // Permutes elements of input vector via bit-reversal permutation
        let pvec = Array.init z (fun i -> vector.[Bit.reverse i depth])

        // Tail-recursive butterfly algorithm
        let rec cleave size =
            if size <= z then
                let mid, step = size / 2, z / size
                let rec condition i =
                    if i < z then
                        let rec flutter idx k =
                            if idx < i + mid then
                                let temp = pvec.[idx + mid] * unity.[k]
                                pvec.[idx + mid] <- (pvec.[idx] - temp)
                                pvec.[idx] <- (pvec.[idx] + temp)
                                flutter (idx + 1) (k + step)
                        flutter i 0; condition (i + size)
                condition 0; cleave (size * 2)
        cleave 2; pvec

    /// <summary>
    /// Chirp Z-transform.
    /// </summary>
    /// <param name="vector"></param>
    /// <param name="parity"></param>
    let rec chirp (parity: Compute) (vector: Complex[]) =
        let z = vector.Length
        let dz = Bit.length (z * 2) |> pown 2

        // Complex roots of unity
        let unity: Complex[] =
            let expn (i: uint64) = 
                double (i * i % (uint64 z * 2UL)) * (coefficient parity) * pi / double z
            Array.init<Complex> z (fun i ->
                Complex.FromPolarCoordinates(1.0, (uint64 i) |> expn))

        // Zero-padded signal vector
        let signal: Complex[] =
            let signal' = Array.map2 (fun x y -> x * y) vector unity
            signal' @ Array.zeroCreate (dz - z)

        // Zero-padded filter vector
        let filter: Complex[] =
            let filter' = unity |> Array.map (fun c -> Complex.Conjugate(c))
            let padding = Array.zeroCreate<Complex> (dz - (2 * z - 1))
            let filter'' = unity.[1..] |> Array.rev 
                           |> Array.map (fun c -> Complex.Conjugate(c))
            filter' @ padding @ filter''

        // Convolution theorem
        let convolve =
            if signal.Length <> filter.Length then 
                failwith "Signal and filter must be of equal lengths"
            let s = signal.Length |> double
            let svec: Complex[] = signal |> compute Compute.Forward
            let fvec: Complex[] = filter |> compute Compute.Forward
            let convolution = Array.map2 (fun x y -> x * y) svec fvec
            convolution 
            |> compute Compute.Reverse 
            |> Array.sub <|| (0, z)
            |> Array.map (fun x -> x ./ s)

        Array.map2 (fun x y -> x * y) convolve unity

    /// <summary>
    /// Initiates FFT computation.
    /// </summary>
    /// <param name="parity"></parity>
    /// <param name="vector"></parity>
    and compute (parity: Compute) (vector: Complex[]) = 
        match vector.Length with
        | 0 | 1 -> vector 
        | Radix2 -> vector |> decimate parity
        | _ -> vector |> chirp parity


module Signal =

    /// <summary>
    /// Computes the Fourier transform.
    /// </summary>
    /// <param name="signal"></param>
    [<CompiledName("FFT")>]
    let fft (signal: double[]) = 
        signal |> Array.map (fun r -> Complex(r, 0.0)) 
               |> Fourier.compute Compute.Forward

    /// <summary>
    /// Computes the inverse Fourier transform.
    /// </summary>
    /// <param name="signal"></param>
    [<CompiledName("IFFT")>]
    let ifft (signal: Complex[]) = 
        // n is a scaling factor
        let n = signal.Length |> double
        signal |> Fourier.compute Compute.Reverse 
               |> Array.map (fun x -> x.Real / n)

    [<CompiledName("TimeShift")>]
    let timeShift (dt: double) (data: double[]) = // dt is the offset ratio
        // pads the input array to the nearest power of 2 for fft efficiency
        // also, define index at which signal may fold into nyquist space
        let nfft = data.Length |> nextpow2
        let nquist = nfft/2+1

        // generates an exponential term for phase shifting the signal in the frequency domain
        // shifts signal towards the time at which reference slice was collected
        let phase (shift: double) = 
            Array.init (nquist) (fun i -> -2. * Math.PI * shift * (double i) / (double nfft))
            |> Array.mapReflect (nquist-1) (nquist-2) (fun x -> -x)
            |> Array.map (fun x -> Complex(cos x, sin x))

        data |> Array.padContinuous nfft
             |> fft 
             |> Array.map2 (fun x y -> x * y) (phase dt)
             |> ifft 
             |> Array.splitAt data.Length |> fst
             |> Array.map (fun x -> abs x)