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
import studentConsulting.model.payload.dto.user.ManageRoleConsultantDTO;
import studentConsulting.model.payload.request.authentication.RoleConsultantRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminRoleConsultantService;

import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminRoleConsultantController {

    @Autowired
    private IAdminRoleConsultantService roleConsultantService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-consultant/list")
    public ResponseEntity<DataResponse<Page<ManageRoleConsultantDTO>>> getRoleConsultants(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId, // thêm roleId vào request
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleConsultantDTO> roleConsultants = roleConsultantService.getAllRoleConsultantsWithFilters(
                Optional.ofNullable(name),
                Optional.ofNullable(roleId),
                pageable);

        if (roleConsultants.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageRoleConsultantDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy role consultant phù hợp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageRoleConsultantDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách role consultant thành công")
                        .data(roleConsultants)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/role-consultant/create")
    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> createRoleConsultant(
            @RequestParam Integer roleId,
            @RequestBody RoleConsultantRequest roleConsultantRequest) {
        if (roleConsultantRequest == null || roleConsultantRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("error")
                            .message("Dữ liệu role consultant không hợp lệ")
                            .build()
            );
        }

        ManageRoleConsultantDTO savedRoleConsultant = roleConsultantService.createRoleConsultant(roleId, roleConsultantRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleConsultantDTO>builder()
                        .status("success")
                        .message("Tạo role consultant thành công")
                        .data(savedRoleConsultant)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/role-consultant/update")
    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> updateRoleConsultant(
            @RequestParam Integer id,
            @RequestParam Integer roleId,
            @RequestBody RoleConsultantRequest roleConsultantRequest) {
        if (roleConsultantRequest == null || roleConsultantRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("error")
                            .message("Dữ liệu role consultant không hợp lệ")
                            .build()
            );
        }

        ManageRoleConsultantDTO updatedRoleConsultant = roleConsultantService.updateRoleConsultant(id, roleId, roleConsultantRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleConsultantDTO>builder()
                        .status("success")
                        .message("Cập nhật role consultant thành công")
                        .data(updatedRoleConsultant)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/role-consultant/delete")
    public ResponseEntity<DataResponse<Void>> deleteRoleConsultant(@RequestParam Integer id) {
        try {
            roleConsultantService.deleteRoleConsultantById(id);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa role consultant thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy role consultant để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-consultant/detail")
    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> getRoleConsultantById(@RequestParam Integer id) {
        try {
            ManageRoleConsultantDTO manageRoleConsultantDTO = roleConsultantService.getRoleConsultantById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("success")
                            .message("Lấy thông tin role consultant thành công")
                            .data(manageRoleConsultantDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("error")
                            .message("Không tìm thấy role consultant")
                            .build()
            );
        }
    }
}
