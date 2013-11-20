defmodule Tools.Maths do

  def std_dev(list) do
    average = :lists.sum(list) / length(list)
    f = fn(x, sum) -> sum + (x - average) * (x - average) end
    variance = :lists.foldl(f, 0.0, list) / length(list)
    :math.sqrt(variance)
  end

  def sample_std_dev(list) do
    average = :lists.sum(list) / length(list)
    f = fn(x, sum) -> sum + (x - average) * (x - average) end
    variance = :lists.foldl(f, 0.0, list) / (length(list) - 1)
    :math.sqrt(variance)
  end

  def floor(x) do
    t = :erlang.trunc(x)
    case (x-t) do
      neg when neg < 0 -> t-1
      pos when pos > 0 -> t
      _ -> t 
    end
  end

end