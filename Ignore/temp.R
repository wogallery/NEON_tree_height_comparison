def product_info(dpID):
    # for each product that contains tiles, profice the information required to
    # 1. build the paths to the data and mosaic
    # 2. default options for resolution and compression and ...
    ## pinfo is dict{list[dict}}, where the list usually has only one element
    pinfo = {
        # RGB tiles
        "DP3.30010.001": #Camera images
            [
                 {"data_name": "Camera",
                 "level": "L3",
                 "tile_re": "*image.tif",
                 "tile_dir": "/Camera/Tiles/",
                 "mos_dir": "/Camera/Mosaic/",
                 "compression": "JPEG",
             "res": 10}],   #reduce resolution by this factor
        "DP3.30015.001": # Ecosystem, CHM)
             [
                 {"data_name": "CHM",
                  "level": "L3",
                  "tile_re": "*CHM.tif",
                  "tile_dir": "/DiscreteLidar/CanopyHeightModelGtif/",
                  "mos_dir":  "/DiscreteLidar/CanopyHeightModelMosaic/",
                  "compression": "LZM",
                  "res": 1}],
        "DP3.30024.001": #Digital surface and terrain models
             [
                 {"data_name": "DSM",
                  "level": "L3",
                  "tile_re": "*DSM.tif",
                  "tile_dir": "/DiscreteLidar/CanopyHeightModelDSMGtif/",
                  "mos_dir":  "/DiscreteLidar/CanopyHeightModelDSMMosaic/",
                  "compression": "LZM",
                  "res": 1},
                 {"data_name": "DTM",
                  "level": "L3",
                  "tile_re": "*DTM.tif",
                  "tile_dir": "/DiscreteLidar/CanopyHeightModelDTMGtif/",
                  "mos_dir": "/DiscreteLidar/CanopyHeightModelDTMMosaic/",
                  "compression": "LZM",
                  "res": 1}]
    }
    return pinfo[dpID]

def get_product_dir(dpID, site, year, domain, visit):
    # e.g.:
    #   "F:\NEON\data\DP3.30010.001\neon-aop-products\2019\FullSite\D17\2019_SJER_4\L3\Camera\Mosaic
    full_site = "_".join(year, site, visit)

    pi = product_info(dpID)
    tile_dir = []
    mos_dir  = []
    level    = []
    for i in range(0, len(pi)):
        tile_dir = tile_dir   +[pi[i]["tile_dir"]]
        mos_dir  = mos_dir    +[pi[1]["mos_dir"]]
        level    = level      +[pi[1]["level"]]
    product_dir = "/".join(".", dpID, "neon-aop-products", year, "FullSite", domain, full_site, level, mos_dir)
