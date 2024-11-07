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
import studentConsulting.model.payload.dto.manage.ManageRoleAskDTO;
import studentConsulting.model.payload.request.RoleAskRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminRoleAskService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminRoleAskController {

    @Autowired
    private IAdminRoleAskService roleAskService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-ask/list")
    public ResponseEntity<DataResponse<Page<ManageRoleAskDTO>>> getRoleAsks(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId, // Thêm roleId vào request
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleAskDTO> roleAsks = roleAskService.getRoleAskByAdmin(name,
                Optional.ofNullable(roleId),
                pageable
        );

        if (roleAsks.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageRoleAskDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy role ask phù hợp")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageRoleAskDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách role ask thành công")
                        .data(roleAsks)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/role-ask/create")
    public ResponseEntity<DataResponse<ManageRoleAskDTO>> createRoleAsk(
            @RequestBody RoleAskRequest roleAskRequest) {

        if (roleAskRequest == null || roleAskRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("error")
                            .message("Dữ liệu role ask không hợp lệ")
                            .build()
            );
        }

        ManageRoleAskDTO savedRoleAsk = roleAskService.createRoleAsk(roleAskRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleAskDTO>builder()
                        .status("success")
                        .message("Tạo role ask thành công")
                        .data(savedRoleAsk)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/role-ask/update")
    public ResponseEntity<DataResponse<ManageRoleAskDTO>> updateRoleAsk(
            @RequestParam Integer id,
            @RequestBody RoleAskRequest roleAskRequest) {
        if (roleAskRequest == null || roleAskRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("error")
                            .message("Dữ liệu role ask không hợp lệ")
                            .build()
            );
        }

        ManageRoleAskDTO updatedRoleAsk = roleAskService.updateRoleAsk(id, roleAskRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageRoleAskDTO>builder()
                        .status("success")
                        .message("Cập nhật role ask thành công")
                        .data(updatedRoleAsk)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/role-ask/delete")
    public ResponseEntity<DataResponse<Void>> deleteRoleAsk(@RequestParam Integer id) {
        try {
            roleAskService.deleteRoleAskById(id);
            return ResponseEntity.ok(
                    DataResponse.<Void>builder()
                            .status("success")
                            .message("Xóa role ask thành công")
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Void>builder()
                            .status("error")
                            .message("Không tìm thấy role ask để xóa")
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-ask/detail")
    public ResponseEntity<DataResponse<ManageRoleAskDTO>> getRoleAskById(@RequestParam Integer id) {
        try {
            ManageRoleAskDTO manageRoleAskDTO = roleAskService.getRoleAskById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("success")
                            .message("Lấy thông tin role ask thành công")
                            .data(manageRoleAskDTO)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageRoleAskDTO>builder()
                            .status("error")
                            .message("Không tìm thấy role ask")
                            .build()
            );
        }
    }
}
