package studentConsulting.controller;

import java.security.Principal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
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
    
    @GetMapping("/list/consultant")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByConsultant(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) Boolean statusPublic,
            @RequestParam(required = false) Boolean statusConfirmed,
            @RequestParam(required = false) Boolean mode,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        // Lấy username của tư vấn viên đang đăng nhập
        String consultantUsername = principal.getName();

        // Tìm thông tin tư vấn viên dựa vào username
        UserInformationEntity consultant = userService.findByUsername(consultantUsername)
                .orElseThrow(() -> new RuntimeException("Tư vấn viên không tồn tại"));

        // Tạo đối tượng Pageable cho phân trang và sắp xếp
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        // Lấy danh sách lịch tư vấn với các bộ lọc
        Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationsByConsultantWithFilters(
                consultant, title, statusPublic, statusConfirmed, mode, pageable);

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

    @PostMapping("/confirm")
    public ResponseEntity<DataResponse<String>> confirmConsultationSchedule(
            @RequestParam Integer scheduleId,
            @RequestBody ConsultationFeedbackRequest request,
            Principal principal) {

        // Lấy tên tư vấn viên đăng nhập
        String consultantUsername = principal.getName();

        // Tìm thông tin tư vấn viên
        UserInformationEntity consultant = userService.findByUsername(consultantUsername)
                .orElseThrow(() -> new RuntimeException("Tư vấn viên không tồn tại"));

        // Gọi service để xác nhận lịch tư vấn
        consultationScheduleService.confirmConsultationSchedule(scheduleId, request, consultant);

        return ResponseEntity.ok(
            DataResponse.<String>builder()
                .status("success")
                .message("Lịch tư vấn đã được xác nhận.")
                .data("Xác nhận thành công.")
                .build()
        );
    }
}
