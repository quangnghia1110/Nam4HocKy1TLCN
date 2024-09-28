package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.dto.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.repository.ConsultationScheduleRegistrationRepository;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.repository.common.DepartmentRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConsultationScheduleService;
import studentConsulting.specification.ConsultationScheduleRegistrationSpecification;
import studentConsulting.specification.ConsultationScheduleSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;

@Service
public class AdvisorConsultationScheduleServiceImpl implements IAdvisorConsultationScheduleService {

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConsultationScheduleRegistrationRepository consultationScheduleRegistrationRepository;

    @Override
    public Page<ConsultationScheduleDTO> getConsultationsByDepartmentWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<ConsultationScheduleEntity> spec = Specification.where(null);

        if (departmentId != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDepartment(departmentId));
        }

        if (title != null && !title.isEmpty()) {
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

        spec = spec.and(ConsultationScheduleSpecification.isNotCreatedByRole("TRUONGBANTUVAN"));

        Page<ConsultationScheduleEntity> schedules = consultationScheduleRepository.findAll(spec, pageable);
        return schedules.map(this::mapToDTO);
    }


    @Override
    @Transactional
    public ManageConsultantScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId, Integer userId) {
        ConsultationScheduleEntity newSchedule = new ConsultationScheduleEntity();

        DepartmentEntity department = departmentRepository.findById(departmentId)
                .orElseThrow(() -> new ErrorException("Phòng ban không tồn tại"));

        newSchedule.setTitle(request.getTitle());
        newSchedule.setContent(request.getContent());
        newSchedule.setConsultationDate(request.getConsultationDate());
        newSchedule.setConsultationTime(request.getConsultationTime());
        newSchedule.setLocation(request.getLocation());
        newSchedule.setLink(request.getLink());
        newSchedule.setMode(request.getMode());
        newSchedule.setStatusPublic(true);
        newSchedule.setStatusConfirmed(true);
        newSchedule.setDepartment(department);
        newSchedule.setCreatedBy(userId);

        ConsultationScheduleEntity savedSchedule = consultationScheduleRepository.save(newSchedule);

        return mapToDTOs(savedSchedule);
    }


    @Override
    public Page<ManageConsultantScheduleDTO> getConsultationsByDepartmentOwnerWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.isCreatedByDepartmentHead());
        if (departmentId != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDepartment(departmentId));
        }

        if (title != null && !title.isEmpty()) {
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

        Page<ConsultationScheduleEntity> schedules = consultationScheduleRepository.findAll(spec, pageable);
        return schedules.map(this::mapToDTOs);
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

    @Override
    public void deleteConsultationSchedule(Integer scheduleId, Integer departmentId) {
        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        if (!schedule.getDepartment().getId().equals(departmentId)) {
            throw new ErrorException("Bạn không có quyền xóa lịch tư vấn này vì nó không thuộc phòng ban của bạn.");
        }

        consultationScheduleRepository.delete(schedule);
    }

    @Override
    @Transactional
    public Page<ConsultationScheduleRegistrationMemberDTO> getMembersByConsultationSchedule(Integer consultationScheduleId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId) {
        consultationScheduleRepository.findById(consultationScheduleId)
                .orElseThrow(() -> new ErrorException("Buổi tư vấn không tồn tại."));

        boolean isOwnedByUser = consultationScheduleRepository.existsByIdAndCreatedBy(consultationScheduleId, userId);
        if (!isOwnedByUser) {
            throw new ErrorException("Buổi tư vấn này không thuộc về bạn.");
        }
        Specification<ConsultationScheduleRegistrationEntity> spec = Specification
                .where(ConsultationScheduleRegistrationSpecification.hasConsultationSchedule(consultationScheduleId))
                .and(ConsultationScheduleRegistrationSpecification.hasStatus(true))
                .and(ConsultationScheduleRegistrationSpecification.hasCreatedBy(userId));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasStartDateAfterOrEqual(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasEndDateBeforeOrEqual(endDate));
        }

        Page<ConsultationScheduleRegistrationEntity> registrations = consultationScheduleRegistrationRepository.findAll(spec, pageable);

        return registrations.map(registration -> {
            String userName = registration.getUser().getLastName() + " " + registration.getUser().getFirstName();

            return ConsultationScheduleRegistrationMemberDTO.builder()
                    .userName(userName)
                    .registeredAt(registration.getRegisteredAt())
                    .status(registration.getStatus())
                    .build();
        });
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
