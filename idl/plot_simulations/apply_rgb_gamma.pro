function apply_rgb_gamma,x,r
        return,255*((x/255.)^r)
end
