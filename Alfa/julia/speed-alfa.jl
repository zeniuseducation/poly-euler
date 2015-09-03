include("Common.jl")

function sol142(lim :: Int)
    for x = 1:lim
        for y = 1:x-1
            if ispsqr(x+y)
                if ispsqr(x-y)
                    for z = 1:y-1
                        if ispsqr(x-z)
                            if ispsqr(x+z)
                                if ispsqr(y-z)
                                    if ispsqr(y+z)
                                        return x+y+z
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    "Ga nemu"
end

    















































# save
