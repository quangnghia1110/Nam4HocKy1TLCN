package studentConsulting.controller;

import java.security.Principal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IConsultationScheduleService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class ConsultationScheduleController {

    @Autowired
    private IConsultationScheduleService consultationScheduleService;

    @Autowired
    private IUserService userService;

    @PreAuthorize("hasRole('USER')")
    @PostMapping("/user/consultation-schedule/create")
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
    
    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/consultation-schedule/list")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getFilterScheduleByUser(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String title,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String username = principal.getName();

        UserInformationEntity user = userService.findByUsername(username)
                .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getSchedulesByUserWithFilters(user, departmentId, title, pageable);

        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(
                DataResponse.<Page<ConsultationScheduleDTO>>builder()
                    .status("error")
                    .message("Không tìm thấy lịch tư vấn.")
                    .build()
            );
        }

        return ResponseEntity.ok(
            DataResponse.<Page<ConsultationScheduleDTO>>builder()
                .status("success")
                .message("Lấy danh sách lịch tư vấn thành công.")
                .data(schedules)
                .build()
        );
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/consultation-schedule/list")
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

        String consultantUsername = principal.getName();

        UserInformationEntity consultant = userService.findByUsername(consultantUsername)
                .orElseThrow(() -> new RuntimeException("Tư vấn viên không tồn tại"));

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationsByConsultantWithFilters(
                consultant, title, statusPublic, statusConfirmed, mode, pageable);

        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(
                DataResponse.<Page<ConsultationScheduleDTO>>builder()
                    .status("error")
                    .message("Không tìm thấy lịch tư vấn.")
                    .build()
            );
        }

        return ResponseEntity.ok(
            DataResponse.<Page<ConsultationScheduleDTO>>builder()
                .status("success")
                .message("Lấy danh sách lịch tư vấn thành công.")
                .data(schedules)
                .build()
        );
    }
    
    @PreAuthorize("hasRole('TUVANVIEN')")
    @PostMapping("/consultant/consultation-schedule/confirm")
    public ResponseEntity<DataResponse<String>> confirmConsultationSchedule(
            @RequestParam Integer scheduleId,
            @RequestBody ConsultationFeedbackRequest request,
            Principal principal) {

        String consultantUsername = principal.getName();

        UserInformationEntity consultant = userService.findByUsername(consultantUsername)
                .orElseThrow(() -> new RuntimeException("Tư vấn viên không tồn tại"));

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
