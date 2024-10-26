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
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.service.interfaces.consultant.IConsultantConsultationScheduleService;

import java.security.Principal;
import java.time.LocalDate;
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
    @PostMapping(value = "/consultant/consultation-schedule/confirm", consumes = {"application/json"})
    public DataResponse<ManageConsultantScheduleDTO> confirmConsultationScheduleForConsultant(
            @RequestParam("scheduleId") Integer scheduleId,
            @RequestBody UpdateConsultationScheduleRequest scheduleRequest,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        if (!user.getAccount().getRoleConsultant().getName().equals("GIANGVIEN")) {
            throw new ErrorException("Chỉ có giảng viên mới có thể xác nhận lịch tư vấn.");
        }

        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
            throw new ErrorException("Bạn không thể cập nhật lịch tư vấn này vì nó đã được xác nhận.");
        }

        if (!schedule.getConsultant().equals(user) || !schedule.getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền cập nhật lịch tư vấn này");
        }

        ManageConsultantScheduleDTO updatedScheduleDTO = consultationScheduleService.confirmConsultationSchedule(
                scheduleId, user.getAccount().getDepartment().getId(), scheduleRequest);

        return DataResponse.<ManageConsultantScheduleDTO>builder()
                .status("success")
                .message("Xác nhận lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }
}
