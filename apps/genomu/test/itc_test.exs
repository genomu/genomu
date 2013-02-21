defmodule ITC.Test.Model do
  import ExUnit.Assertions
  use Proper.Properties
  alias :proper_statem, as: StateM
  @behaviour StateM
  alias :proper_types, as: Types
  defrecord State, [itcs: []]

  def initial_state do
    {ch, _} = ITC.fork(ITC.seed)
    State.new [itcs: [ch]]
  end

  ## Helper macro
  defmacrop is_debug, do: false
  defmacrop out(msg) do
    if is_debug do
      quote do
        IO.puts unquote(msg)
      end
    end
  end
  defmacrop delegate({name, _, args}) do
    quote do
       __FUN__ = atom_to_binary(unquote(name))
       args = unquote(args)
       res = ITC.unquote(name)(unquote_splicing(args))
       out "command: #{__FUN__}(#{inspect args}) -> #{inspect res}"
       res
    end
  end
  defmacrop validate(message, result) do
    __FUN__ = atom_to_binary(elem(__CALLER__.function, 0))
    quote do
      result = unquote(result)
      case result do
        true -> out unquote(__FUN__) <> ": " <> unquote(message) <> " -> true"
        res  -> out unquote(__FUN__) <> ": " <> unquote(message) <> " -> #{inspect res}"
      end
      result
    end
  end
  defmacrop conditional(condition, reply) do
    quote do
      if unquote(condition) do
        {1, unquote(reply)}
      else
        {0, nil}
      end
    end
  end

  # Delegation
  def seed, do: delegate(seed)
  def seed?(t), do: delegate(seed?(t))
  def fork(t), do: delegate(fork(t))
  def join(t1, t2), do: delegate(join(t1, t2))
  def event(t), do: delegate(event(t))
  def leq(t1, t2), do: delegate(leq(t1, t2))
  def le(t1, t2), do: delegate(le(t1, t2))
  def eq(t1, t2), do: delegate(eq(t1, t2))

  defp filter_garbage(elements) do
   Enum.filter elements,
     fn
       ({:var, _}) -> false
       _ -> true
     end
  end

  defp filter(clocks, fun) do
    Enum.reduce clocks, [],
      fn(t1, acc) ->
        Enum.reduce clocks, acc,
          fn(t2, acc) ->
            if fun.(t1, t2), do: acc = [[t1, t2]|acc]
            acc
          end
      end
  end

  def joinable(clocks), do: filter(clocks, ITC.joinable?(&1, &2))
  def comparable(clocks) do
    filter(clocks,
      fn(t1, t2) -> not(ITC.eq(t1, t2)) and not(ITC.concurrent?(t1, t2)) end)
  end

  def command(State[] = state) do
    # we want to filter out proper symbolic execution garbage
    state = state.itcs(filter_garbage(state.itcs))
    {freq_a, a} = conditional(state.itcs != [], hd(state.itcs))

    clocks = joinable(state.itcs)

    {freq_join, join} = conditional(length(clocks) >= 2, Types.oneof(clocks))
    {freq_leq, leq} =
       conditional length(state.itcs) >= 2, Types.vector(2, Types.oneof(state.itcs))

    comparable = comparable(state.itcs)
    {freq_compare, compare} =
       conditional length(comparable) >= 2, Types.oneof(comparable)

    Types.frequency([
       {freq_a * 4, {:call, __MODULE__, :fork, [Types.oneof(state.itcs)]}},
       {freq_join * 6, {:call, __MODULE__, :join, join}},
       {freq_a * 2, {:call, __MODULE__, :event, [a]}},

       {freq_a * 1, {:call, __MODULE__, :leq, [a, a]}},
       {freq_leq * 2, {:call, __MODULE__, :leq, leq}},

       {freq_a * 1, {:call, __MODULE__, :le, [a, a]}},
       {freq_compare * 2, {:call, __MODULE__, :le, compare}},

       {freq_a * 1, {:call, __MODULE__, :eq, [a, a]}},
       {freq_compare * 2, {:call, __MODULE__, :eq, compare}},
    ])
   end

  def precondition(_, {:call, __MODULE__, :join, [t1, t2]}) do
    not(ITC.eq(t1, t2))
  end
  def precondition(_, {:call, __MODULE__, _, _}), do: true

  def precondition(_, _call) do
    out "UNHANDLED precondition: #{inspect _call}"
    true
  end

  def postcondition(_, {:call, __MODULE__, :fork, [t]}, {t1, t2}) do
    validate "fork(#{inspect t}) -> #{inspect {t1, t2}}",
      t1 != t and t2 != t and ITC.join(t1, t2) == t
  end
  def postcondition(_, {:call, __MODULE__, :join, [t1, t2]}, t) do
    {a, b} = ITC.fork(t)
    validate "join(#{inspect t1}, #{inspect t2}) -> #{inspect t}",
      t1 != t and t2 != t and ITC.leq(t1, a) and ITC.leq(t2, b)
  end
  def postcondition(_, {:call, __MODULE__, :event, [t]}, reply) do
    validate "event(#{inspect t}) -> #{inspect reply}",
      reply != t and ITC.leq(t, reply)
  end
  def postcondition(_, {:call, __MODULE__, :leq, [t1, t2]}, reply) do
    validate "leq(#{inspect t1}, #{inspect t2}) -> #{inspect reply}",
      is_boolean(reply)
        and ((reply == true and (not(ITC.le(t2, t1)) or ITC.eq(t1, t2))
          or (reply == false and (ITC.le(t2, t1) or ITC.concurrent?(t1, t2)))))
  end

  def postcondition(_, {:call, __MODULE__, :le, [t1, t2]}, reply) do
    validate "le(#{inspect t1}, #{inspect t2}) -> #{inspect reply}",
      is_boolean(reply)
        and ((reply == true and (not(ITC.le(t2, t1)) and not(ITC.eq(t1, t2))))
          or (reply == false and (ITC.le(t2, t1) or ITC.eq(t1, t2))))
  end

  def postcondition(_, {:call, __MODULE__, :eq, [t1, t2]}, reply) do
    validate "eq(#{inspect t1}, #{inspect t2}) -> #{inspect reply}",
      is_boolean(reply)
        and ((reply == false and (ITC.le(t1, t2) or ITC.le(t2, t1)))
          or (reply == true and (ITC.eq(t2, t1))))
  end

  def postcondition(_, _call, _) do
    out "UNHANDLED: post: #{inspect _call}"
    false
  end

  def next_state(s, _reply, {:call, __MODULE__, :fork, [t]}) do
    {t1, t2} = ITC.fork(t)
    s = s.itcs([t1, t2|s.itcs])
    s
  end
  def next_state(s, _reply, {:call, __MODULE__, :join, [t1, t2]}) do
    s = s.itcs([ITC.join(t1, t2)|(s.itcs -- [t1, t2])])
    s
  end
  def next_state(s, _reply, {:call, __MODULE__, :event, [t]}) do
    s = s.itcs([ITC.event(t)|(s.itcs -- [t])])
    s
  end
  def next_state(s, _reply, {:call, __MODULE__, :leq, [_t1, _t2]}), do: s
  def next_state(s, _reply, {:call, __MODULE__, :le, [_t1, _t2]}), do: s
  def next_state(s, _reply, {:call, __MODULE__, :eq, [_t1, _t2]}), do: s

  def next_state(s, _, _call) do
    out "UNHANDLED: next: #{inspect _call}"
    s
  end

  property "seed/seed\?" do
    forall itc in Types.frequency([{9, Types.term}, {1, ITC.seed}]) do
      is_boolean(ITC.seed?(itc))
    end
  end

  property "itc model properties" do
    forall cmds in StateM.commands(__MODULE__) do
       {h, s, res} = StateM.run_commands(__MODULE__, cmds)
        whenfail report(cmds, h, s, res),
                 aggregate(StateM.command_names(cmds), res === :ok)
    end
  end

  def report(_cmds, _h, _s, _res) do # TODO
  end

end

defmodule ITC.Test.Model.Case do
  use ExUnit.Case

  test "state machine" do
    assert Proper.module(ITC.Test.Model, [numtests: 100]) == []
  end

end
