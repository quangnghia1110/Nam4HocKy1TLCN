package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.manage.ManageUserDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminUserInformationService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminUserInformationController {
    @Autowired
    private IAdminUserInformationService userInformationService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/user-information/list")
    public ResponseEntity<DataResponse<Page<ManageUserDTO>>> getUsers(
            @RequestParam(required = false) Integer accountId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageUserDTO> users = userInformationService.getUserByAdmin(
                accountId,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                pageable);

        if (users.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                    DataResponse.<Page<ManageUserDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy người dùng phù hợp.")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageUserDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách người dùng thành công")
                        .data(users)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/user-information/detail")
    public ResponseEntity<DataResponse<ManageUserDTO>> getUserById(@RequestParam Integer id) {
        try {
            ManageUserDTO userInformation = userInformationService.getUserById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageUserDTO>builder()
                            .status("success")
                            .message("Lấy thông tin người dùng thành công")
                            .data(userInformation)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageUserDTO>builder()
                            .status("error")
                            .message("Không tìm thấy người dùng với ID: " + id)
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/user-information/update")
    public ResponseEntity<DataResponse<ManageUserDTO>> updateUserInformation(
            @RequestParam Integer id,
            @RequestBody ManageUserDTO userRequest) {
        try {
            ManageUserDTO updatedUser = userInformationService.updateUserInformation(id, userRequest);
            return ResponseEntity.ok(
                    DataResponse.<ManageUserDTO>builder()
                            .status("success")
                            .message("Cập nhật thông tin người dùng thành công")
                            .data(updatedUser)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(
                    DataResponse.<ManageUserDTO>builder()
                            .status("error")
                            .message(e.getMessage())
                            .build()
            );
        }
    }

}
