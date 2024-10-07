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
import studentConsulting.model.payload.dto.address.ManageWardDTO;
import studentConsulting.model.payload.request.address.WardRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminWardService;

import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminWardController {

    @Autowired
    private IAdminWardService wardService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/ward/list")
    public ResponseEntity<DataResponse<Page<ManageWardDTO>>> getWards(
            @RequestParam String districtCode,
            @RequestParam(required = false) Optional<String> code,
            @RequestParam(required = false) Optional<String> name,
            @RequestParam(required = false) Optional<String> nameEn,
            @RequestParam(required = false) Optional<String> fullName,
            @RequestParam(required = false) Optional<String> fullNameEn,
            @RequestParam(required = false) Optional<String> codeName,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "code") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ManageWardDTO> wards = wardService.getAllWardsWithFilters(code, name, nameEn, fullName, fullNameEn, codeName, Optional.of(districtCode), pageable);

        if (wards.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageWardDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy Phường/Xã phù hợp trong quận/huyện")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageWardDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách Phường/Xã thành công")
                        .data(wards)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/ward/create")
    public ResponseEntity<DataResponse<ManageWardDTO>> createWard(
            @RequestParam String districtCode,
            @RequestParam String code,
            @RequestBody WardRequest wardRequest) {
        if (wardRequest == null || districtCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Dữ liệu Phường/Xã hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        if (wardService.existsByCode(code)) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Mã Phường/Xã đã tồn tại")
                            .build()
            );
        }

        ManageWardDTO savedWard = wardService.createWard(code, districtCode, wardRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageWardDTO>builder()
                        .status("success")
                        .message("Tạo Phường/Xã thành công")
                        .data(savedWard)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/ward/update")
    public ResponseEntity<DataResponse<ManageWardDTO>> updateWard(
            @RequestParam String code,
            @RequestParam String districtCode,
            @RequestBody WardRequest wardRequest) {
        if (wardRequest == null || districtCode == null || code == null) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Dữ liệu Phường/Xã hoặc mã quận/huyện không hợp lệ")
                            .build()
            );
        }

        ManageWardDTO updatedWard = wardService.updateWard(code, districtCode, wardRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageWardDTO>builder()
                        .status("success")
                        .message("Cập nhật Phường/Xã thành công")
                        .data(updatedWard)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/ward/delete")
    public ResponseEntity<DataResponse<Void>> deleteWard(@RequestParam String code) {
        try {
            wardService.deleteWardByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa Phường/Xã thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy Phường/Xã để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/ward/detail")
    public ResponseEntity<DataResponse<ManageWardDTO>> getWardByCode(@RequestParam String code) {
        try {
            ManageWardDTO manageWardDTO = wardService.getWardByCode(code);
            return ResponseEntity.ok(
                    DataResponse.<ManageWardDTO>builder()
                            .status("success")
                            .message("Lấy thông tin Phường/Xã thành công")
                            .data(manageWardDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageWardDTO>builder()
                            .status("error")
                            .message("Không tìm thấy Phường/Xã")
                            .build()
            );
        }
    }
}
