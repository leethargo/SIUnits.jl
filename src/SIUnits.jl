isdefined(Base, :__precompile__) && __precompile__()

module SIUnits

    using Compat; import Compat.String

    import Base: ==, +, -, *, /, .+, .-, .*, ./, //, ^
    import Base: promote_rule, promote_type, convert, show, mod

    struct SIQuantity{T<:Number,m,kg,s,A,K,mol,cd,rad,sr} <: Number
        val::T
    end

    UnitQuantity{T} =  SIQuantity{T,0,0,0,0,0,0,0,0,0}

    SIQuantity(x::T) where {T<:Number} = UnitQuantity{T}(x)

    struct SIUnit{m,kg,s,A,K,mol,cd,rad,sr} <: Number
    end

    if !isdefined(Base, :UnitRange)
        const Range = Ranges # Deprecations introduced early in the 0.3 cycle
        const UnitRange = Range1
    end

    abstract type SIRanges{T,m,kg,s,A,K,mol,cd,rad,sr} <: Range{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}} end

    struct SIRange{R<:Range,T<:Real,m,kg,s,A,K,mol,cd,rad,sr} <: SIRanges{T,m,kg,s,A,K,mol,cd,rad,sr}
        val::R
    end

    const UnitTuple = NTuple{9,Int}

    unit(x::SIRanges{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = SIUnit{m,kg,s,A,K,mol,cd,rad,sr}()
    quantity(x::SIRanges{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}

    import Base: length, getindex, unsafe_getindex, next, float, show, start, step, last, done, first, eltype, one, zero

    one(x::SIQuantity) = one(x.val)
    one(::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = one(T)
    zero(x::SIQuantity) = zero(x.val) * unit(x)
    zero(::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = zero(T) * SIUnit{m,kg,s,A,K,mol,cd,rad,sr}()

    # This is all nessecary because SIQuanity{T<:Real} !<: Real
    show(io::IO, x::SIRanges) = (show(io, x.val); show(io,unit(x)))
    function show(io::IO, r::SIRange)
        if step(r) == zero(quantity(r))
            print(io, "SIRange(",first(r),",",step(r),",",length(r),")")
        else
            print(io, first(r),':',step(r),':',last(r))
        end
    end
    show(io::IO, r::SIRange{T}) where {T<:UnitRange} = print(io, first(r),':',last(r))
    getindex(r::SIRanges,i::Integer) = (quantity(r)(getindex(r.val,i)))
    getindex(r::T,i::Range) where {T<:SIRanges} = T(getindex(r.val,i))
    unsafe_getindex(r::SIRanges,i::Integer) = (quantity(r)(unsafe_getindex(r.val,i)))
    unsafe_getindex(r::T,i::Range) where {T<:SIRanges} = T(unsafe_getindex(r.val,i))
    function next(r::SIRanges, i)
        v, j = next(r.val,i)
        to_q(quantity(r),v), j
    end
    length(r::SIRanges) = length(r.val)
    start(r::SIRanges) = start(r.val)
    done(r::SIRanges,i) = done(r.val,i)
    eltype(r::SIRanges) = quantity(r)

    for func in (:first,:step,:last)
        @eval $(func)(r::SIRanges) = to_q(quantity(r),$(func)(r.val))
    end
    # Forward some linear range transformations to the wrapped range
    rangequantity(::Type{R},tup::UnitTuple) where {R<:Range} = SIRange{R,eltype(R),tup[1],tup[2],tup[3],tup[4],tup[5],tup[6],tup[7],tup[8],tup[9]}
    for func in (VERSION < v"0.3-" ? (:+, :-) : (:.+, :.-)) # version 0.3 swaps fallbacks
        @eval $(func)(x::SIRanges{T,m,kg,s,A,K,mol,cd,rad,sr}, y::SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr} = (val = $(func)(x.val, y.val); SIRange{typeof(val),eltype(val),m,kg,s,A,K,mol,cd,rad,sr}(val))
        @eval $(func)(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}, y::SIRanges{S,m,kg,s,A,K,mol,cd,rad,sr}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr} = (val = $(func)(x.val, y.val); SIRange{typeof(val),eltype(val),m,kg,s,A,K,mol,cd,rad,sr}(val))
    end
    ./(x::SIRanges, y::SIQuantity) = (val = ./(x.val, y.val); rangequantity(typeof(val),tup(x)-tup(y))(val))
    .*(x::SIRanges, y::SIQuantity) = (val = .*(x.val, y.val); rangequantity(typeof(val),tup(x)+tup(y))(val))
    .*(x::SIQuantity, y::SIRanges) = (val = .*(x.val, y.val); rangequantity(typeof(val),tup(x)+tup(y))(val))
    # Version 0.2 assumes all Ranges have start and len fields in ==, and
    # the fallback in 0.3 needlessly iterates through all values
    ==(r::SIRanges, s::SIRanges) = r.val == s.val && tup(r) == tup(s)
    ==(s::SIRanges, r::Range) = s.val == r && tup(s) == (0,0,0,0,0,0,0,0,0)
    ==(r::Range, s::SIRanges) = r == s.val && tup(s) == (0,0,0,0,0,0,0,0,0)

    tup2u(tup) = SIUnit{tup[1],tup[2],tup[3],tup[4],tup[5],tup[6],tup[7],tup[8],tup[9]}
    quantity(T::Type,tup::UnitTuple) = quantity(T,tup2u(tup)())
    -(tup::UnitTuple) = (-tup[1],-tup[2],-tup[3],-tup[4],-tup[5],-tup[6],-tup[7],-tup[8],-tup[9])

    for op in (:-,:*,:+)
        @eval function $(op)(tup1::UnitTuple,tup2::UnitTuple)
            ($(op)(tup1[1],tup2[1]),$(op)(tup1[2],tup2[2]),$(op)(tup1[3],tup2[3]),$(op)(tup1[4],tup2[4]),$(op)(tup1[5],tup2[5]),
                $(op)(tup1[6],tup2[6]),$(op)(tup1[7],tup2[7]),$(op)(tup1[8],tup2[8]),$(op)(tup1[9],tup2[9]))
        end
    end

    export quantity, @quantity

    function quantity(T,quant::SIQuantity{S}) where S
        quant.val == one(S) || error("Quantity value must be unity!")
        quantity(T,unit(quant))
    end
    quantity(T::(Union{Type,TypeVar}),unit::SIUnit{m,kg,s,A,K,mol,cd,rad,sr}) where {m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}

    tup(u::SIUnit{m,kg,s,A,K,mol,cd,rad,sr}) where {m,kg,s,A,K,mol,cd,rad,sr} = (m,kg,s,A,K,mol,cd,rad,sr)
    tup(u::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = (m,kg,s,A,K,mol,cd,rad,sr)
    tup(u::SIRanges{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = (m,kg,s,A,K,mol,cd,rad,sr)

    macro quantity(expr,unit)
        esc(:(SIUnits.SIQuantity{$expr,SIUnits.tup($unit)...}))
    end

    # Irrationals propagate through units. Fancy!!
    promote_rule(x::Type{Irrational{sym}},y::Type{SIUnit{m,kg,s,A,K,mol,cd,rad,sr}}) where {sym,m,kg,s,A,K,mol,cd,rad,sr} =
        SIQuantity{Irrational{sym},m,kg,s,A,K,mol,cd,rad,sr}
    promote_rule(x::Type{Irrational{sym}},y::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}}) where {sym,T,m,kg,s,A,K,mol,cd,rad,sr} =
        SIQuantity{promote_type(Irrational{sym},T)}

    promote_rule(
A::Type{SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}},B::Type{SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}}) where {T,S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS,mT,kgT,sT,AT,KT,molT,cdT,radT,srT} = SIQuantity{promote_type(T,S)}
    promote_rule(
A::Type{SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}},B::Type{SIUnit{mS,kgS,sS,AS,KS,molS,cdS,radS,srS}}) where {T,mS,kgS,sS,AS,KS,molS,cdS,radS,srS,mT,kgT,sT,AT,KT,molT,cdT,radT,srT} = SIQuantity{T}
    promote_rule(x::Type{Bool},y::Type{SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}}) where {S,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{promote_type(Bool,S)}
    promote_rule(x::Type{Bool},y::Type{SIUnit{m,kg,s,A,K,mol,cd,rad,sr}}) where {m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{Bool}
    promote_rule(x::Type{T},y::Type{SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}}) where {T<:Number,S,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{promote_type(T,S)}
    promote_rule(x::Type{T},y::Type{SIUnit{m,kg,s,A,K,mol,cd,rad,sr}}) where {T<:Number,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{T}

    # One unspecified, units, one concrete (unspecified occurs as the promotion result from the rules above)
    promote_rule(x::Type{SIQuantity{T}},y::Type{SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{promote_type(T,S)}

    # Unlike most other types, the promotion of two identitical SIQuantities is
    # not that type itself. As such, the promote_type behavior itself must be
    # overridden. C.f. https://github.com/Keno/SIUnits.jl/issues/27
    promote_type(::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}}, ::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{T}

    eval(quote
    convert(::Type{SIQuantity{T}},x::Dates.Period) where {T} = error("Conversion from Period to SIQuantity not defined")
end)
    convert(::Type{SIQuantity{T}},x::SIUnit{m,kg,s,A,K,mol,cd,rad,sr}) where {T<:Number,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}(one(T))
    convert(::Type{SIQuantity{T}},x::T) where {T<:Number} = UnitQuantity{T}(x)
    convert(::Type{SIQuantity{T}},x::S) where {T<:Number,S<:Number} = convert(SIQuantity{T},convert(T,x))
    convert(::Type{SIQuantity{T}},x::SIQuantity{T}) where {T<:Number} = x
    convert(::Type{SIQuantity{T}},x::SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}) where {T<:Number,S,m,kg,s,A,K,mol,cd,rad,sr} = SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}(convert(T,x.val))

    to_q(::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}},val::T) where {T,m,kg,s,A,K,mol,cd,rad,sr} = (0 == m == kg == s == A == K == mol == cd == rad == sr) ? val : SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}(val)
    convert(::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}},val::Number) where {T,m,kg,s,A,K,mol,cd,rad,sr} = (SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}(convert(T,val)))
    function convert(::Type{SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}},val::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}) where {T,S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}
        if mS != mT || kgS != kgT || sS != sT || AS != AT || KS != KT || molS != molT || cdS != cdT || radS != radT || srS != srT
            error("Dimension mismatch in convert. Attempted to convert a ($(repr(SIUnit{mS,kgS,sS,AS,KS,molS,cdS,radS,srS}))) to ($(repr(SIUnit{mT,kgT,sT,AT,KT,molT,cdT,radT,srT})))")
        end
        SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}(convert(T,val.val))
    end

    for op in (:/,://)

        @eval function ($op)(x::Number,y::SIQuantity{T}) where T
            val = ($op)(x,y.val)
            to_q(quantity(typeof(val),-tup(y)),val)
        end

        @eval function ($op)(x::SIQuantity,y::SIQuantity)
            val = $(op)(x.val,y.val)
            to_q(quantity(typeof(val),tup(x)-tup(y)),val)
        end

        @eval $(op)(x::SIUnit,y::SIUnit) = tup2u(tup(x)-tup(y))()
        @eval $(op)(x::SIQuantity{T},y::SIUnit) where {T} = to_q(quantity(T,tup(unit(x))-tup(y)),x.val)
        @eval $(op)(x::SIUnit,y::SIQuantity{T}) where {T} = to_q(quantity(T,tup(x)-tup(unit(y))),($op)(1,y.val))

        @eval $(op)(x::Number,y::SIUnit) = x*tup2u(-tup(y))()
    end

    inv(y::SIUnit) = tup2u(-tup(y))()

    function +(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},y::SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr}
        val = x.val+y.val
        SIQuantity{typeof(val),m,kg,s,A,K,mol,cd,rad,sr}(val)
    end

    function -(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},y::SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr}
        val = x.val-y.val
        SIQuantity{typeof(val),m,kg,s,A,K,mol,cd,rad,sr}(val)
    end

    function -(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}) where {T,S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}
        error("Unit mismatch. Got ($(repr(unit(x)))) - ($(repr(unit(y))))")
    end

    function +(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}) where {T,S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}
        error("Unit mismatch. Got ($(repr(unit(x)))) + ($(repr(unit(y))))")
    end

    function -(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        val = -(x.val)
        SIQuantity{typeof(val),m,kg,s,A,K,mol,cd,rad,sr}(val)
    end

    function ^(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},i::Integer) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        if i == 0
            return one(T)
        end
        val = x.val^i
        SIQuantity{typeof(val),m*i,kg*i,s*i,A*i,K*i,mol*i,cd*i,rad*i,sr*i}(val)
    end

    function ^(
        x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},r::Rational) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        if r == 0
            return one(T)
        end
        val = x.val^r
        SIQuantity{typeof(val),convert(Int,m*r),convert(Int,kg*r),convert(Int,s*r),convert(Int,A*r),
        convert(Int,K*r),convert(Int,mol*r),convert(Int,cd*r),convert(Int,rad*r),convert(Int,sr*r)}(val)
    end

    ^(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},r::AbstractFloat) where {T,m,kg,s,A,K,mol,cd,rad,sr} = x^rationalize(r)

    function ^(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}) where {T,S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}
        error("Can not raise a number to a unitful quantity. Got ($(repr(unit(x))))^($(repr(unit(y))))")
    end

    ^(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},y::SIQuantity{S,0,0,0,0,0,0,0,0}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr} = x.val^(y.val)

    ==(x::SIQuantity,y::SIQuantity) = (tup(x) == tup(y)) && (x.val == y.val)
    ==(x::SIQuantity{T},y::SIUnit) where {T} = (tup(x) == tup(y)) && (x.val == one(T))
    ==(x::SIUnit,y::SIQuantity{T}) where {T} = (tup(x) == tup(y)) && (one(T) == y.val)
    ==(x::SIUnit,y::SIUnit) = tup(x) == tup(y)

    import Base: sqrt, abs, abs2, colon, isless, isfinite, isreal, real, imag, isnan

    function colon(start::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},step::SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr},stop::SIQuantity{X,m,kg,s,A,K,mol,cd,rad,sr}) where {T,S,X,m,kg,s,A,K,mol,cd,rad,sr}
        val = colon(start.val,step.val,stop.val)
        SIRange{typeof(val),eltype(val),m,kg,s,A,K,mol,cd,rad,sr}(val)
    end

    function colon(start::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},stop::SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr}
        val = colon(start.val,stop.val)
        SIRange{typeof(val),eltype(val),m,kg,s,A,K,mol,cd,rad,sr}(val)
    end

    function sqrt(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        val = sqrt(x.val)
        SIQuantity{typeof(val),convert(Int,m/2),convert(Int,kg/2),convert(Int,s/2),convert(Int,A/2),
        convert(Int,K/2),convert(Int,mol/2),convert(Int,cd/2),convert(Int,rad/2),convert(Int,rad/2)}(val)
    end

    function abs(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}(abs(x.val))
    end

    function abs2(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        val = abs2(x.val)
        SIQuantity{typeof(val),2*m,2*kg,2*s,2*A,2*K,2*mol,2*cd,2*rad,2*sr}(val)
    end

    function isfinite(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        isfinite(x.val)
    end

    isnan(x::SIQuantity) = isnan(x.val)
    isreal(x::SIQuantity) = isreal(x.val)
    real(x::SIQuantity) = typeof(x)(real(x.val))
    imag(x::SIQuantity) = typeof(x)(imag(x.val))

    function isless(x::SIQuantity{T,0,0,0,0,0,0,0,0,0}, y::SIQuantity{T,0,0,0,0,0,0,0,0,0}) where T
        return isless(x.val,y.val)
    end
    function isless(x::SIQuantity{T,0,0,0,0,0,0,0,0,0}, y::SIQuantity{S,0,0,0,0,0,0,0,0,0}) where {T,S}
        return isless(x.val,y.val)
    end
    function isless(x::SIQuantity{T,0,0,0,0,0,0,0,0,0}, y::Real) where T
        return isless(x.val,y)
    end
    function isless(x::Real, y::SIQuantity{T,0,0,0,0,0,0,0,0,0}) where T
        return isless(x,y.val)
    end
    function isless(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT},y::SIQuantity{S,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}) where {T,S,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}
        return isless(x.val,y.val)
    end

    function mod(
        x::SIQuantity{T,mT,kgT,sT,AT,KT,molT,cdT,radT,srT},y::SIQuantity{S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS}) where {T,S,mS,kgS,sS,AS,KS,molS,cdS,radS,srS,mT,kgT,sT,AT,KT,molT,cdT,radT,srT}
        error("Unit mismatch. Got mod($(repr(unit(x))),$(repr(unit(y))))")
    end

    function mod(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr},y::SIQuantity{S,m,kg,s,A,K,mol,cd,rad,sr}) where {T,S,m,kg,s,A,K,mol,cd,rad,sr}
        val = mod(x.val,y.val)
        SIQuantity{typeof(val),m,kg,s,A,K,mol,cd,rad,sr}(val)
    end

    import Base: sin, cos, tan, cot, sec, csc
    for func in (:sin,:cos,:tan,:cot,:sec,:csc)
        @eval $func(θ::SIQuantity{T,0,0,0,0,0,0,0,1,0}) where {T} = $func(θ.val)
    end

    # Forwarding methods that do not affect units
    import Base: conj
    conj(x::SIQuantity) = typeof(x)(conj(x.val))

    float(x::SIQuantity) = float(x.val)
    Base.Float64(x::SIQuantity) = Float64(x.val)
Base.Int(x::SIQuantity) = Int(x.val)

    *(x::SIUnit,y::SIUnit) = tup2u(tup(x)+tup(y))()
    *(x::SIUnit,y::SIQuantity{T}) where {T} = to_q(quantity(T,tup(y)+tup(x)),y.val)
    *(x::SIQuantity{T},y::SIUnit) where {T} = to_q(quantity(T,tup(y)+tup(x)),x.val)
    *(x::Irrational,y::SIUnit) = quantity(typeof(x),y)(x)
    function *(x::SIQuantity,y::SIQuantity)
        ret = x.val * y.val
        to_q(quantity(typeof(ret),tup(x)+tup(y)),ret)
    end


    function ^(
        x::SIUnit{m,kg,s,A,K,mol,cd,rad,sr},i::Integer) where {m,kg,s,A,K,mol,cd,rad,sr}
        SIUnit{m*i,kg*i,s*i,A*i,K*i,mol*i,cd*i,rad*i,sr*i}()
    end

    unit(x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr} = SIUnit{m,kg,s,A,K,mol,cd,rad,sr}()

    export SIPrefix, Meter, KiloGram, Second, Ampere, Kelvin, Mole, Candela, Radian, Steradian, Kilo, Mega, Giga,
        Tera, Peta, Exa, Zetta, Centi, Milli, Micro, Nano, Pico, Femto, Atto, Zepto, Yocto,
        Gram, Joule, Coulomb, Volt, Farad, Newton, Ohm, CentiMeter, Siemens, Hertz, Watt, Pascal

    const SIPrefix  = SIUnit{0,0,0,0,0,0,0,0,0}()
    const Meter     = SIUnit{1,0,0,0,0,0,0,0,0}()
    const KiloGram  = SIUnit{0,1,0,0,0,0,0,0,0}()
    const Second    = SIUnit{0,0,1,0,0,0,0,0,0}()
    const Ampere    = SIUnit{0,0,0,1,0,0,0,0,0}()
    const Kelvin    = SIUnit{0,0,0,0,1,0,0,0,0}()
    const Mole      = SIUnit{0,0,0,0,0,1,0,0,0}()
    const Candela   = SIUnit{0,0,0,0,0,0,1,0,0}()
    const Radian    = SIUnit{0,0,0,0,0,0,0,1,0}()
    const Steradian = SIUnit{0,0,0,0,0,0,0,0,1}()

    const Kilo       = (1000)SIPrefix
    const Mega       = (10^6)SIPrefix
    const Giga       = (10^9)SIPrefix
    const Tera       = (10^12)SIPrefix
    const Peta       = (10^15)SIPrefix
    const Exa        = (10^18)SIPrefix
    const Zetta      = (10^21)SIPrefix
    const Yotta      = (10^24)SIPrefix
    const Centi      = (1//100)SIPrefix
    const Milli      = (1//1000)SIPrefix
    const Micro      = (1//10^6)SIPrefix
    const Nano       = (1//10^9)SIPrefix
    const Pico       = (1//10^12)SIPrefix
    const Femto      = (1//10^15)SIPrefix
    const Atto       = (1//10^18)SIPrefix
    const Zepto      = (1//10^21)SIPrefix
    const Yocto      = (1//10^24)SIPrefix

    const Gram       = (1//1000)KiloGram
    const Joule      = KiloGram*Meter^2/Second^2
    const Coulomb    = Ampere*Second
    const Volt       = Joule/Coulomb
    const Farad      = Coulomb^2/Joule
    const Newton     = KiloGram*Meter/Second^2
    const Ohm        = Volt/Ampere
    const Hertz      = inv(Second)
    const Siemens    = inv(Ohm)
    const Watt       = Joule/Second
    const Pascal     = Newton/Meter^2

    const CentiMeter = Centi*Meter


# Pretty Printing - Text
    superscript(i) = map(repr(i)) do c
        c   ==  '-' ? '\u207b' :
        c   ==  '1' ? '\u00b9' :
        c   ==  '2' ? '\u00b2' :
        c   ==  '3' ? '\u00b3' :
        c   ==  '4' ? '\u2074' :
        c   ==  '5' ? '\u2075' :
        c   ==  '6' ? '\u2076' :
        c   ==  '7' ? '\u2077' :
        c   ==  '8' ? '\u2078' :
        c   ==  '9' ? '\u2079' :
        c   ==  '0' ? '\u2070' :
        error("Unexpected Chatacter")
    end

    function spacing(idx::Int, x::SIUnit)
        # Only print a space if there are nonzero units coming after this one
        tup(x)[idx+1:end] == ntuple((i)->0, 9-idx) ? "" : " "
    end
    function show(io::IO,x::SIUnit{m,kg,s,A,K,mol,cd,rad,sr}) where {m,kg,s,A,K,mol,cd,rad,sr}
        kg  != 0 && print(io, "kg",  (kg  == 1 ? spacing(1,x) : superscript(kg)))
        m   != 0 && print(io, "m",   (m   == 1 ? spacing(2,x) : superscript(m)))
        s   != 0 && print(io, "s",   (s   == 1 ? spacing(3,x) : superscript(s)))
        A   != 0 && print(io, "A",   (A   == 1 ? spacing(4,x) : superscript(A)))
        K   != 0 && print(io, "K",   (K   == 1 ? spacing(5,x) : superscript(K)))
        mol != 0 && print(io, "mol", (mol == 1 ? spacing(6,x) : superscript(mol)))
        cd  != 0 && print(io, "cd",  (cd  == 1 ? spacing(7,x) : superscript(cd)))
        rad != 0 && print(io, "rad", (rad == 1 ? spacing(8,x) : superscript(rad)))
        sr  != 0 && print(io, "sr",  (sr  == 1 ? ""           : superscript(sr)))
        nothing
    end

    function show(io::IO,x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        show(io,x.val)
        print(io," ")
        show(io,unit(x))
    end

    function sidims(::SIUnit{m,kg,s,A,K,mol,cd,rad,sr}) where {m,kg,s,A,K,mol,cd,rad,sr}
        (m,kg,s,A,K,mol,cd,rad,sr)
    end

    function sidims(::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        (m,kg,s,A,K,mol,cd,rad,sr)
    end

    export @prettyshow

    macro prettyshow(unit,string)
        esc(quote function Base.show(io::IO,::SIUnits.SIUnit{SIUnits.sidims($(unit))...})
            print(io,$(string))
        end
        function Base.show(io::IO,::MIME"text/mathtex+latex",::SIUnits.SIUnit{SIUnits.sidims($(unit))...})
            show(io,MIME("text/mathtex+latex"),$(string))
        end
        end)
    end

# Pretty Printing - LaTeX

    using TexExtensions

    import Base: show

    macro l(x)
        esc(quote
            $x != 0 && push!($x>0?num:den,string("\\text{",$(string(x)),"\}",abs($x) == 1 ? " " : string("^{",abs($x),"}")))
        end)
    end

    function show(io::IO,::MIME"text/mathtex+latex",x::SIUnit{m,kg,s,A,K,mol,cd,rad,sr}) where {m,kg,s,A,K,mol,cd,rad,sr}
        num = String[]
        den = String[]
        @l kg
        @l m
        @l s
        @l A
        @l K
        @l mol
        @l cd
        @l rad
        @l sr
        if !isempty(den)
            if isempty(num)
                write(io,"\\frac{1}{",join(den,"\\;"),"}")
            else
                write(io,"\\frac{",join(num,"\\;"),"}{",join(den,"\\;"),"}")
            end
        else
            write(io,join(num,"\\;"))
        end
    end

    function show(io::IO,::MIME"text/mathtex+latex",x::SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}) where {T,m,kg,s,A,K,mol,cd,rad,sr}
        show(io,MIME("text/mathtex+latex"),x.val)
        write(io,"\\;")
        show(io,MIME("text/mathtex+latex"),unit(x))
    end

# Non-SI Units
struct NonSIUnit{BaseUnit<:SIUnit,Unit}; end
struct NonSIQuantity{T<:Number,Unit<:NonSIUnit} <: Number
    val::T
end

# Non-SI promote rules
promote_rule(x::Type{Irrational},y::Type{NonSIUnit}) =
    NonSIQuantity{x,y}
promote_rule(x::Type{Irrational{sym}},y::Type{NonSIQuantity{T,Unit}}) where {sym,T<:Number,Unit} =
    NonSIQuantity{promote_type(Irrational{sym},T),Unit}

promote_rule(
A::Type{NonSIQuantity{T,U1}},B::Type{SIQuantity{S,U2}}) where {T<:Number,S<:Number,U1,U2} = NonSIQuantity{promote_type(T,S)}
promote_rule(
A::Type{NonSIQuantity{T,U1}},U2::Type{NonSIUnit}) where {T<:Number,U1} = NonSIQuantity{T}
promote_rule(x::Type{Bool},y::Type{NonSIQuantity{S,U}}) where {S<:Number,U} = NonSIQuantity{promote_type(Bool,S),U}
promote_rule(x::Type{Bool},U::Type{NonSIUnit}) = NonSIQuantity{Bool,U}
promote_rule(x::Type{T},y::Type{NonSIQuantity{S,U}}) where {T<:Number,S<:Number,U} = NonSIQuantity{promote_type(T,S),U}
promote_rule(x::Type{T},U::Type{NonSIUnit}) where {T<:Number} = NonSIQuantity{T,U}

# Interaction between SI and non-SI quantities
promote_rule(x::Type{NonSIQuantity{S,U}},y::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}}) where {S<:Number,T<:Number,U,m,kg,s,A,K,mol,cd,rad,sr} =
    SIQuantity{promote_type(S,T)}
promote_rule(x::Type{SIQuantity{T,m,kg,s,A,K,mol,cd,rad,sr}},y::Type{NonSIQuantity{S,U}}) where {S<:Number,T<:Number,U,m,kg,s,A,K,mol,cd,rad,sr} =
    SIQuantity{promote_type(S,T)}

siquantity(T,U::NonSIUnit{B}) where {B} = quantity(T,B())
siquantity(T,U::Type{NonSIUnit{B}}) where {B} = quantity(T,B())
#convert{T,S,U}(::Type{SIQuantity{T}},x::NonSIQuantity{S,U}) = (siquantity(promote_type(T,S),U())(x.val))


*(x::Number,t::T) where {T<:NonSIUnit} = NonSIQuantity{typeof(x),T}(x)

baseunit(x::NonSIUnit{BaseUnit}) where {BaseUnit} = BaseUnit()
baseunit(x::NonSIQuantity{T,Unit}) where {T,Unit} = baseunit(unit(x))
unit(x::NonSIQuantity{T,Unit}) where {T,Unit} = Unit()
quantity(T::(Union{Type,TypeVar}),x::NonSIUnit) = NonSIQuantity{T,typeof(x)}
quantity(T::(Union{Type,TypeVar}),U::Type{NonSIUnit}) = NonSIQuantity{T,U}

/(x::NonSIQuantity{T,U},y::SIQuantity) where {T,U} = convert(SIQuantity,x)/y
/(x::NonSIUnit,y::SIUnit) = convert(SIQuantity,x)/y
/(x::SIUnit,y::NonSIUnit) = x/convert(SIQuantity,y)

/(x::SIQuantity,y::NonSIUnit) = x/convert(SIQuantity,y)
/(x::NonSIUnit,y::SIQuantity) = convert(SIQuantity,x)/y
-(x::NonSIQuantity{T,U}) where {T,U} = NonSIQuantity{T,U}(-x.val)

^(x::(Union{NonSIQuantity,NonSIUnit}),i::Integer) = convert(SIQuantity,x)^i

show(io::IO,x::NonSIUnit{BaseUnit,Unit}) where {BaseUnit,Unit} = write(io,string(Unit))
function show(io::IO,x::NonSIQuantity)
    show(io,x.val)
    print(io," ")
    show(io,unit(x))
end

function show(io::IO,::MIME"text/mathtex+latex",x::NonSIUnit{BaseUnit,Unit}) where {BaseUnit,Unit}
    write(io,"\\text{",string(Unit),"}")
end

function show(io::IO,::MIME"text/mathtex+latex",x::NonSIQuantity)
    show(io,MIME("text/mathtex+latex"),x.val)
    write(io,"\\;")
    show(io,MIME("text/mathtex+latex"),unit(x))
end

# No, these two are not the same. Sometimes we get SIQuantities that are not specialized
# on the type out of promotion, hence the first method, but we still need the second method
# to be more specific that the convert methods of plain SIUnits above.
convert(::Type{SIQuantity},x::NonSIQuantity) = x.val * convert(SIQuantity,unit(x))
convert(::Type{SIQuantity{T}},x::NonSIQuantity) where {T<:Number} = x.val * convert(SIQuantity,unit(x))

convert(::Type{NonSIQuantity{T}},U::NonSIUnit) where {T} = NonSIQuantity{T,U}(one(T))
convert(::Type{NonSIQuantity{T,U}},x::T) where {T<:Number,U} = UnitQuantity{T}(x)
#convert{T}(::Type{NonSIQuantity{T}},x::T) = UnitQuantity{T}(x)
#convert{T,S}(::Type{NonSIQuantity{T}},x::S) = convert(NonSIQuantity{T},convert(T,x))
eval(quote
    convert(::Type{NonSIQuantity{T,U}},x::Dates.Period) where {T,U} = error("Conversion from Period to NonSIQuantity not defined")
end)
convert(::Type{NonSIQuantity{T,U}},x::Number) where {T,U} = convert(NonSIQuantity{T,U},convert(T,x))
convert(::Type{NonSIQuantity{T,U}},x::NonSIQuantity{T,U}) where {T,U} = x
convert(::Type{NonSIQuantity{T,U1}},x::NonSIQuantity{S,U2}) where {T<:Number,S,U1,U2} = NonSIQuantity{T,U2}(convert(T,x.val))

export as

function as(x::SIQuantity,y::U) where U<:NonSIUnit
    val = x/y
    @assert !(typeof(val)<:SIQuantity)
    NonSIQuantity{typeof(val),U}(val)
end

function as(X::AbstractArray{Q},y::U) where {U<:NonSIUnit,Q<:SIQuantity}
    val = [x/y for x in X]
    @assert !(typeof(eltype(val))<:SIQuantity)
    NonSIQuantity{typeof(val),U}(val)
end

function as(x::NonSIQuantity,y::SIUnit)
    @assert baseunit(x) == y
    convert(SIQuantity,x)
end

as(x::NonSIQuantity,y::SIQuantity) = as(x,unit(y))

include("nonsiunits.jl")
include("shortunits.jl")

end # module
