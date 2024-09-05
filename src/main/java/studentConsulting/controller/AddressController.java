package studentConsulting.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.payload.dto.DistrictDTO;
import studentConsulting.model.payload.dto.ProvinceDTO;
import studentConsulting.model.payload.dto.WardDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IAddressService;

@RestController
@RequestMapping("/api/v1/address")
public class AddressController {

    @Autowired
    private IAddressService addressService;

    @GetMapping("/provinces")
    public ResponseEntity<DataResponse<List<ProvinceDTO>>> getAllProvinces() {
        List<ProvinceDTO> provinces = addressService.getAllProvinces();
        DataResponse<List<ProvinceDTO>> response = DataResponse.<List<ProvinceDTO>>builder()
                .status("success")
                .message("Fetched all provinces successfully.")
                .data(provinces)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/districts/{provinceCode}")
    public ResponseEntity<DataResponse<List<DistrictDTO>>> getDistrictsByProvince(@PathVariable String provinceCode) {
        List<DistrictDTO> districts = addressService.getDistrictsByProvince(provinceCode);
        DataResponse<List<DistrictDTO>> response = DataResponse.<List<DistrictDTO>>builder()
                .status("success")
                .message("Fetched districts for province code: " + provinceCode + " successfully.")
                .data(districts)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/wards/{districtCode}")
    public ResponseEntity<DataResponse<List<WardDTO>>> getWardsByDistrict(@PathVariable String districtCode) {
        List<WardDTO> wards = addressService.getWardsByDistrict(districtCode);
        DataResponse<List<WardDTO>> response = DataResponse.<List<WardDTO>>builder()
                .status("success")
                .message("Fetched wards for district code: " + districtCode + " successfully.")
                .data(wards)
                .build();

        return ResponseEntity.ok(response);
    }
}
