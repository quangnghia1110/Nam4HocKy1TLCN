package studentConsulting.controller;

import java.security.Principal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IConsultationScheduleService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("/api/v1/consultationSchedule")
public class ConsultationScheduleController {

    @Autowired
    private IConsultationScheduleService consultationScheduleService;

    @Autowired
    private IUserService userService;

    @PostMapping("/create")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> createConsultation(
            @RequestBody CreateScheduleConsultationRequest request,
            Principal principal) {
        
        String username = principal.getName();

        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("Người dùng không tồn tại"));

        ConsultationScheduleDTO createdSchedule = consultationScheduleService.createConsultation(request, user);

        return ResponseEntity.ok(
            DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Lịch tư vấn đã được tạo thành công.")
                .data(createdSchedule)
                .build()
        );
    }
    
    @GetMapping("/list/user")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByUser(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String title,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        // Lấy username của người dùng đang đăng nhập
        String username = principal.getName();

        // Tìm thông tin người dùng dựa vào username
        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new RuntimeException("Người dùng không tồn tại"));

        // Tạo đối tượng Pageable cho phân trang và sắp xếp
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleDTO> schedules;

        // Kiểm tra điều kiện lọc theo departmentId và title
        if (departmentId != null && title != null) {
            schedules = consultationScheduleService.getConsultationsByUserAndDepartmentAndTitle(user, departmentId, title, pageable);
        } else if (departmentId != null) {
            schedules = consultationScheduleService.getConsultationsByUserAndDepartment(user, departmentId, pageable);
        } else if (title != null) {
            schedules = consultationScheduleService.searchConsultationsByUserAndTitle(user, title, pageable);
        } else {
            schedules = consultationScheduleService.getAllConsultationsByUser(user, pageable);
        }

        // Nếu không tìm thấy lịch tư vấn nào, trả về lỗi 404
        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(
                DataResponse.<Page<ConsultationScheduleDTO>>builder()
                    .status("error")
                    .message("No consultation schedules found.")
                    .build()
            );
        }

        // Trả về kết quả thành công với dữ liệu lịch tư vấn
        return ResponseEntity.ok(
            DataResponse.<Page<ConsultationScheduleDTO>>builder()
                .status("success")
                .message("Fetched consultation schedules successfully.")
                .data(schedules)
                .build()
        );
    }

}
