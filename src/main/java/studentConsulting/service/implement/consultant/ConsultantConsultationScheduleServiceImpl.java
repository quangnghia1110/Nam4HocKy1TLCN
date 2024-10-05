package studentConsulting.service.implement.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRegistrationRepository;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.service.interfaces.consultant.IConsultantConsultationScheduleService;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleSpecification;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class ConsultantConsultationScheduleServiceImpl implements IConsultantConsultationScheduleService {

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConsultationScheduleRegistrationRepository consultationScheduleRegistrationRepository;

    @Override
    public Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(
            UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,
            LocalDate startDate, LocalDate endDate, Pageable pageable) {

        if (!consultant.getAccount().getRoleConsultant().getName().equals("GIANGVIEN")) {
            throw new ErrorException("Chỉ có giảng viên mới có thể xem lịch tư vấn.");
        }

        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasConsultant(consultant));

        if (title != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasTitle(title));
        }

        if (statusPublic != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusPublic(statusPublic));
        }

        if (statusConfirmed != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusConfirmed(statusConfirmed));
        }

        if (mode != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasMode(mode));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDateBefore(endDate));
        }

        return consultationScheduleRepository.findAll(spec, pageable).map(this::mapToDTO);
    }


    @Override
    public void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request, UserInformationEntity consultant) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
            errors.add(new FieldErrorDetail("schedule", "Lịch đã được xác nhận trước đó."));
        }

        if (!consultant.getAccount().getRoleConsultant().getName().equals("GIANGVIEN")) {
            throw new ErrorException("Chỉ có giảng viên mới có thể xác nhận lịch tư vấn.");
        }


        if (request.getStatusConfirmed()) {
            if (schedule.getMode() != null && schedule.getMode()) { // Online
                if (request.getLocation() != null) {
                    errors.add(new FieldErrorDetail("schedule", "Không được phép nhập địa điểm cho tư vấn online."));
                }
                if (request.getLink() == null || request.getConsulationDate() == null || request.getConsultationTime() == null) {
                    errors.add(new FieldErrorDetail("schedule", "Phải cung cấp đầy đủ thông tin link, ngày và giờ cho tư vấn online."));
                }
                schedule.setLink(request.getLink());
            } else { // Offline
                if (request.getLink() != null) {
                    errors.add(new FieldErrorDetail("schedule", "Không được phép nhập link cho tư vấn offline."));
                }
                if (request.getLocation() == null || request.getConsulationDate() == null || request.getConsultationTime() == null) {
                    errors.add(new FieldErrorDetail("schedule", "Phải cung cấp đầy đủ thông tin địa điểm, ngày và giờ cho tư vấn offline."));
                }
                schedule.setLocation(request.getLocation());
            }
            schedule.setStatusConfirmed(true);
            schedule.setConsultationDate(request.getConsulationDate());
            schedule.setConsultationTime(request.getConsultationTime());
        } else {
            schedule.setStatusConfirmed(false);
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        consultationScheduleRepository.save(schedule);
    }

    @Override
    public Optional<ConsultationScheduleEntity> findConsulationScheduleById(Integer scheduleId) {
        return consultationScheduleRepository.findConsulationScheduleById(scheduleId);
    }

    @Override
    public ManageConsultantScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request) {
        ConsultationScheduleEntity existingSchedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        if (!existingSchedule.getDepartment().getId().equals(departmentId)) {
            throw new ErrorException("Bạn không có quyền cập nhật lịch tư vấn này vì nó không thuộc phòng ban của bạn.");
        }

        existingSchedule.setTitle(request.getTitle());
        existingSchedule.setContent(request.getContent());
        existingSchedule.setConsultationDate(request.getConsultationDate());
        existingSchedule.setConsultationTime(request.getConsultationTime());
        existingSchedule.setLocation(request.getLocation());
        existingSchedule.setLink(request.getLink());
        existingSchedule.setMode(request.getMode());
        existingSchedule.setStatusPublic(request.getStatusPublic());
        existingSchedule.setStatusConfirmed(request.getStatusConfirmed());

        ConsultationScheduleEntity updatedSchedule = consultationScheduleRepository.save(existingSchedule);

        return mapToDTOs(updatedSchedule);
    }

    private ConsultationScheduleDTO mapToDTO(ConsultationScheduleEntity schedule) {
        return ConsultationScheduleDTO.builder()
                .id(schedule.getId())
                .department(schedule.getDepartment() != null
                        ? new DepartmentDTO(
                        schedule.getDepartment().getId(),
                        schedule.getDepartment().getName()
                )
                        : null)
                .title(schedule.getTitle())
                .content(schedule.getContent())
                .consultationDate(schedule.getConsultationDate())
                .consultationTime(schedule.getConsultationTime())
                .location(schedule.getLocation())
                .link(schedule.getLink())
                .mode(schedule.getMode())
                .statusPublic(schedule.getStatusPublic())
                .statusConfirmed(schedule.getStatusConfirmed())
                .consultantName(schedule.getConsultant().getLastName() + " " + schedule.getConsultant().getFirstName())
                .userName(schedule.getUser().getLastName() + " " + schedule.getUser().getFirstName())
                .createdBy(schedule.getUser().getId())
                .build();
    }

    private ManageConsultantScheduleDTO mapToDTOs(ConsultationScheduleEntity schedule) {
        return ManageConsultantScheduleDTO.builder()
                .id(schedule.getId())
                .title(schedule.getTitle())
                .content(schedule.getContent())
                .consultationDate(schedule.getConsultationDate())
                .consultationTime(schedule.getConsultationTime())
                .location(schedule.getLocation())
                .link(schedule.getLink())
                .mode(schedule.getMode())
                .statusPublic(schedule.getStatusPublic())
                .statusConfirmed(schedule.getStatusConfirmed())
                .created_by(schedule.getCreatedBy())
                .build();
    }

}