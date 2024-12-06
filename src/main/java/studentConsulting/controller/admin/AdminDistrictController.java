package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.manage.ManageDistrictDTO;
import studentConsulting.model.payload.request.DistrictRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminDistrictService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IPdfService;

@RestController
@RequestMapping("${base.url}")
public class AdminDistrictController {

    @Autowired
    private IAdminDistrictService districtService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/district/list")
    public ResponseEntity<DataResponse<Page<ManageDistrictDTO>>> getDistricts(
            @RequestParam(required = false) String provinceCode,
            @RequestParam(required = false) String code,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String nameEn,
            @RequestParam(required = false) String fullName,
            @RequestParam(required = false) String fullNameEn,
            @RequestParam(required = false) String codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageDistrictDTO> districts = districtService.getDistrictByAdmin(code, name, nameEn, fullName, fullNameEn, codeName, provinceCode, pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<ManageDistrictDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách quận/huyện thành công")
                        .data(districts)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/district/create")
    public ResponseEntity<DataResponse<ManageDistrictDTO>> createDistrict(
            @RequestParam String provinceCode,
            @RequestParam String code,
            @RequestBody DistrictRequest districtRequest) {
        if (districtRequest == null || provinceCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("error")
                            .message("Dữ liệu quận/huyện, mã tỉnh hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        if (districtService.existsByCode(code)) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("error")
                            .message("Mã quận/huyện đã tồn tại")
                            .build()
            );
        }

        ManageDistrictDTO savedDistrict = districtService.createDistrict(code, provinceCode, districtRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageDistrictDTO>builder()
                        .status("success")
                        .message("Tạo quận/huyện thành công")
                        .data(savedDistrict)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/district/update")
    public ResponseEntity<DataResponse<ManageDistrictDTO>> updateDistrict(
            @RequestParam String code,
            @RequestParam String provinceCode,
            @RequestBody DistrictRequest districtRequest) {
        if (districtRequest == null || provinceCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("error")
                            .message("Dữ liệu quận/huyện, mã tỉnh hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        ManageDistrictDTO updatedDistrict = districtService.updateDistrict(code, provinceCode, districtRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageDistrictDTO>builder()
                        .status("success")
                        .message("Cập nhật quận/huyện thành công")
                        .data(updatedDistrict)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/district/delete")
    public ResponseEntity<DataResponse<Void>> deleteDistrict(@RequestParam String code) {
        try {
            districtService.deleteDistrictByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa quận/huyện thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy quận/huyện để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/district/detail")
    public ResponseEntity<DataResponse<ManageDistrictDTO>> getDistrictByCode(@RequestParam String code) {
            ManageDistrictDTO manageDistrictDTO = districtService.getDistrictByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<ManageDistrictDTO>builder()
                            .status("success")
                            .data(manageDistrictDTO)
                            .build()
            );

    }
}
