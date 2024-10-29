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
import studentConsulting.model.payload.dto.address.ManageProvinceDTO;
import studentConsulting.model.payload.request.address.ProvinceRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminProvinceService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminProvinceController {

    @Autowired
    private IAdminProvinceService provinceService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/province/list")
    public ResponseEntity<DataResponse<Page<ManageProvinceDTO>>> getProvinces(
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

        Page<ManageProvinceDTO> provinces = provinceService.getAllProvincesWithFilters(
                Optional.ofNullable(code),
                Optional.ofNullable(name),
                Optional.ofNullable(nameEn),
                Optional.ofNullable(fullName),
                Optional.ofNullable(fullNameEn),
                Optional.ofNullable(codeName),
                pageable
        );

        if (provinces.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageProvinceDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy tỉnh/thành phù hợp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageProvinceDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách tỉnh/thành phố thành công")
                        .data(provinces)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/province/create")
    public ResponseEntity<DataResponse<ManageProvinceDTO>> createProvince(
            @RequestParam String code,
            @RequestBody ProvinceRequest provinceRequest) {
        if (provinceRequest == null || code == null || code.trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Dữ liệu tỉnh/thành không hợp lệ hoặc thiếu mã")
                            .build()
            );
        }

        if (provinceService.existsByCode(code)) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Mã tỉnh/thành đã tồn tại")
                            .build()
            );
        }

        ManageProvinceDTO savedProvince = provinceService.createProvince(provinceRequest, code);

        return ResponseEntity.ok(
                DataResponse.<ManageProvinceDTO>builder()
                        .status("success")
                        .message("Tạo tỉnh/thành phố thành công")
                        .data(savedProvince)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/province/update")
    public ResponseEntity<DataResponse<ManageProvinceDTO>> updateProvince(@RequestParam String code, @RequestBody ProvinceRequest provinceRequest) {
        if (provinceRequest == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Dữ liệu tỉnh/thành không hợp lệ")
                            .build()
            );
        }

        ManageProvinceDTO updatedProvince = provinceService.updateProvince(code, provinceRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageProvinceDTO>builder()
                        .status("success")
                        .message("Cập nhật tỉnh/thành phố thành công")
                        .data(updatedProvince)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/province/delete")
    public ResponseEntity<DataResponse<Void>> deleteProvince(@RequestParam String code) {
        try {
            provinceService.deleteProvinceByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa tỉnh/thành phố thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy tỉnh/thành phố để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/province/detail")
    public ResponseEntity<DataResponse<ManageProvinceDTO>> getProvinceByCode(@RequestParam String code) {
        try {
            ManageProvinceDTO ManageProvinceDTO = provinceService.getProvinceByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("success")
                            .message("Lấy thông tin tỉnh/thành phố thành công")
                            .data(ManageProvinceDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageProvinceDTO>builder()
                            .status("error")
                            .message("Không tìm thấy tỉnh/thành phố")
                            .build()
            );
        }
    }
}
