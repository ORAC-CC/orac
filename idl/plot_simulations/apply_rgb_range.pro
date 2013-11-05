function apply_rgb_range,x,r
        return,interpol_rs([0.,255],r,x)
end
