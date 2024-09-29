package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.service.interfaces.consultant.IConsultantConsultationScheduleService;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantConsultationScheduleController {

    @Autowired
    private IConsultantConsultationScheduleService consultationScheduleService;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/consultation-schedule/list")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByConsultant(
            @RequestParam(required = false) String title, @RequestParam(required = false) Boolean statusPublic,
            @RequestParam(required = false) Boolean statusConfirmed, @RequestParam(required = false) Boolean mode,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy, @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationsByConsultantWithFilters(
                user, title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("error")
                    .message("Không tìm thấy lịch tư vấn.").build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("success")
                .message("Lấy danh sách lịch tư vấn thành công.").data(schedules).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping("/consultant/consultation-schedule/confirm")
    public ResponseEntity<DataResponse<String>> confirmConsultationSchedule(@RequestParam Integer scheduleId,
                                                                            @RequestBody ConsultationFeedbackRequest request, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity consultant = userOpt.get();

        consultationScheduleService.confirmConsultationSchedule(scheduleId, request, consultant);

        ConsultationScheduleEntity schedule = consultationScheduleService.findConsulationScheduleById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        UserInformationEntity user = schedule.getUser();

        NotificationEntity notification = NotificationEntity.builder()
                .senderId(consultant.getId())
                .receiverId(user.getId())
                .content(NotificationContent.CONFIRM_CONSULATION_SCHEDULE.formatMessage(user.getLastName() + " " + user.getFirstName()))
                .time(LocalDateTime.now())
                .notificationType(NotificationType.USER)
                .status(NotificationStatus.UNREAD)
                .build();

        NotificationResponseDTO.NotificationDTO notificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                .senderId(notification.getSenderId())
                .receiverId(notification.getReceiverId())
                .content(notification.getContent())
                .time(notification.getTime())
                .notificationType(notification.getNotificationType().name())
                .status(notification.getStatus().name())
                .build();

        NotificationResponseDTO responseDTO = NotificationResponseDTO.builder()
                .status("notification")
                .data(notificationDTO)
                .build();

        notificationService.sendNotification(notificationDTO);
        System.out.println("Payload: " + responseDTO);

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(user.getId()), "/notification", responseDTO);

        return ResponseEntity
                .ok(DataResponse.<String>builder().status("success").message("Lịch tư vấn đã được xác nhận.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PutMapping(value = "/consultant/consultation-schedule/update", consumes = {"multipart/form-data"})
    public DataResponse<ManageConsultantScheduleDTO> updateConsultationScheduleForConsultant(
            @RequestParam("scheduleId") Integer scheduleId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("consultationDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate consultationDate,
            @RequestParam("consultationTime") String consultationTime,
            @RequestParam("location") String location,
            @RequestParam("link") String link,
            @RequestParam("mode") Boolean mode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestParam("statusConfirmed") Boolean statusConfirmed,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
            throw new ErrorException("Bạn không thể cập nhật lịch tư vấn này vì nó đã được xác nhận.");
        }

        if (!schedule.getConsultant().equals(user) || !schedule.getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền cập nhật lịch tư vấn này");
        }

        UpdateConsultationScheduleRequest scheduleRequest = UpdateConsultationScheduleRequest.builder()
                .title(title)
                .content(content)
                .consultationDate(consultationDate)
                .consultationTime(consultationTime)
                .location(location)
                .link(link)
                .mode(mode)
                .statusPublic(statusPublic)
                .statusConfirmed(statusConfirmed)
                .build();

        ManageConsultantScheduleDTO updatedScheduleDTO = consultationScheduleService.updateConsultationSchedule(scheduleId, user.getAccount().getDepartment().getId(), scheduleRequest);

        return DataResponse.<ManageConsultantScheduleDTO>builder()
                .status("success")
                .message("Cập nhật lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }
}
