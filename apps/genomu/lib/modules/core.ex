defmodule Genomu.Module.Core do
  use Genomu.Module, id: 0, name: :core

  @false_value MsgPack.pack(false)

  @args 0
  @doc """
  <refbody>
   <section>
    This operation returns the value unmodified. That is, <codeph>identity(value) == value</codeph>.

    It is mainly used to retrieve the actual value of the object:

    <codeblock>
      GET(key, core.identity())
    </codeblock>

   </section>
  </refbody>
  <related-links>
    <linkpool type="reference">
     <link href="core_identity_1.dita"/>
    </linkpool>
  </related-links>
  """
  def identity(value, _no_arg, _opts) do
    value
  end

  @args 1
  @name :identity
  @doc """
  <refbody>
   <section>
    <parml>
     <plentry>
       <pt>new_value</pt>
       <pd>New value</pd>
     </plentry>
    </parml>
    This operation replaces the value with another value. That is, <codeph>identity(value, new_value) == new_value</codeph>.

    It is mainly used to replace the value of the object:

    <codeblock>
      SET(key, core.identity(new_value))
    </codeblock>
   </section>
  </refbody>
  <related-links>
    <linkpool type="reference">
     <link href="core_identity_0.dita"/>
    </linkpool>
  </related-links>
  """
  def set_identity(_value, new_value, _opts) do
    new_value
  end

  @args 2
  @doc """
  <refbody>
   <section>
    <parml>
     <plentry>
       <pt>f</pt>
       <pd>Second operation</pd>
     </plentry>
     <plentry>
       <pt>g</pt>
       <pd>First operation</pd>
     </plentry>
    </parml>
    This operation composes two operations into one: <codeph>compose(f(),g())</codeph> is a representation
    of <codeph>f(g())</codeph>
   </section>
   <example>
     In the example below one composes <codeph>list.length(dict.keys())</codeph> in order
     to calculate the number of keys in the dictionary:

     <codeblock>
       core.compose(list.length(), dict.keys())
     </codeblock>
   </example>
  </refbody>
  <related-links>
    <linkpool type="concept">
     <link href="http://en.wikipedia.org/wiki/Function_composition_(computer_science)" format="html"/>
    </linkpool>
  </related-links>
  """
  def compose(value, MsgPack.fix_array(len: 2) = arr, opts) do
    {[bin1, bin2], ""} = MsgPack.unpack(arr)
    {op1, ""} = Genomu.Operation.next(bin1)
    {op2, ""} = Genomu.Operation.next(bin2)
    value = Genomu.Operation.apply(op2, value, opts)
    Genomu.Operation.apply(op1, value, opts)
  end

  @args 1
  @doc """
  <refbody>
   <section>
    <parml>
     <plentry>
       <pt>op</pt>
       <pd>Operation to assert</pd>
     </plentry>
    </parml>
    This operation is an important building block for managing concurrent transactions.
    <apiname>assert</apiname> evaluates the operation in the argument and aborts if the
    result of the operation is false. Any other value returned by the operation in the
    argument makes <apiname>assert</apiname> return the original value.

    <note type="important">
    An interesting (and special) property of this operation is that even if it has not
    aborted the flow within a transaction scope, it may abort it during the commit phase
    if the new value constructed during recalculation does not pass this assertion.
    Due to the way commit phase is constructed, this operation has to be applied either
    with SET or APPLY command. If it is applied with GET, it will only be executed
    withing the transaction scope, but will not be included into the commit phase.
    </note>

   </section>
   <example>
     In the example below one asserts that the value of the object is nil. This is a common
     pattern to ensure the value of the object is not overwritten.

     <codeblock>
       APPLY(key, core.assert(boolean.equals(nil)))
       APPLY(key, core.identity(value))
       COMMIT
     </codeblock>

     If two concurrent transactions will try commit this transaction, only one of them
     (whichever will arrive first) will set key's value, the other one will abort.
   </example>
  </refbody>
  """
  def assert(value, op, opts) do
    {op, ""} = MsgPack.unpack(op)
    if Genomu.Operation.apply(op, value, opts) == @false_value do
      raise Genomu.Operation.AbortException
    end
    value
  end

  @args 0
  @doc """
  <refbody>
   <section>
   This operation will return a binary representation of the current version of the object
   </section>
  </refbody>
  """
  def version(_, _, opts) do
    MsgPack.pack(opts[:version])
  end

  @args 0
  @doc """
  <refbody>
   <section>
   This operation will return a dictionary with the representation of the last operation applied
   to the object
   </section>
  </refbody>
  """
  def operation(_, _, opts) do
    {{module, operation, arg}, _} = Genomu.Operation.deserialize(opts[:operation])
    {arg, _} = MsgPack.unpack(arg)
    MsgPack.pack(MsgPack.Map.from_list([{"module", Genomu.Module.id(module)},
                                        {"operation", Genomu.Module.operation(module, {:name, operation})[:id]},
                                        {"argument", arg}
                                       ]))
  end

end