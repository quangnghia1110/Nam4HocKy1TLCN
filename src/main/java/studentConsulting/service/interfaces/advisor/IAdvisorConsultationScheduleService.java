package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;

import java.time.LocalDate;
import java.util.List;

public interface IAdvisorConsultationScheduleService {

    Page<ConsultationScheduleDTO> getConsultationsByDepartmentWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ManageConsultantScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId, Integer userId);

    Page<ManageConsultantScheduleDTO> getConsultationsByDepartmentOwnerWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ManageConsultantScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request);

    void deleteConsultationSchedule(Integer scheduleId, Integer departmentId);

    Page<ConsultationScheduleRegistrationMemberDTO> getMembersByConsultationSchedule(Integer consultationScheduleId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId);

    ConsultationScheduleDTO getConsultationScheduleByIdAndDepartment(Integer scheduleId, Integer departmentId);

    ManageConsultantScheduleDTO getConsultationScheduleByIdAndCreatedBy(Integer scheduleId, Integer createdById);

    Page<ConsultationScheduleDTO> getAllConsultationSchedulesWithFilters(String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ManageConsultantScheduleDTO getConsultationScheduleByIds(Integer scheduleId);

    void deleteConsultationScheduleAsAdmin(Integer scheduleId);

    ManageConsultantScheduleDTO updateConsultationScheduleAsAdmin(Integer scheduleId, UpdateConsultationScheduleRequest request);

    Page<ManageConsultantScheduleDTO> getAllConsultationsWithFilters(String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ConsultationScheduleDTO getConsultationScheduleById(Integer scheduleId);

    void importConsultationSchedules(List<List<String>> csvData);

    void importManageConsultantSchedules(List<List<String>> csvData);

}

