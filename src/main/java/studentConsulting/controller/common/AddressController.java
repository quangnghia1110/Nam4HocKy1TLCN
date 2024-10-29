package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.model.payload.dto.address.DistrictDTO;
import studentConsulting.model.payload.dto.address.ProvinceDTO;
import studentConsulting.model.payload.dto.address.WardDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.common.IAddressService;

import java.util.List;

@RestController
@RequestMapping("${base.url}")
public class AddressController {

    @Autowired
    private IAddressService addressService;

    @GetMapping("/address/provinces")
    public ResponseEntity<DataResponse<List<ProvinceDTO>>> getAllProvinces() {
        List<ProvinceDTO> provinces = addressService.getAllProvinces();
        DataResponse<List<ProvinceDTO>> response = DataResponse.<List<ProvinceDTO>>builder()
                .status("success")
                .message("Lấy thành công danh sách các tỉnh/thành phố.")
                .data(provinces)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/address/districts")
    public ResponseEntity<DataResponse<List<DistrictDTO>>> getDistrictsByProvince(@RequestParam String provinceCode) {
        List<DistrictDTO> districts = addressService.getDistrictsByProvince(provinceCode);
        DataResponse<List<DistrictDTO>> response = DataResponse.<List<DistrictDTO>>builder()
                .status("success")
                .message("Lấy thành công danh sách quận/huyện cho mã tỉnh: " + provinceCode)
                .data(districts)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/address/wards")
    public ResponseEntity<DataResponse<List<WardDTO>>> getWardsByDistrict(@RequestParam String districtCode) {
        List<WardDTO> wards = addressService.getWardsByDistrict(districtCode);
        DataResponse<List<WardDTO>> response = DataResponse.<List<WardDTO>>builder()
                .status("success")
                .message("Lấy thành công danh sách phường/xã cho mã quận/huyện: " + districtCode)
                .data(wards)
                .build();

        return ResponseEntity.ok(response);
    }
}
