package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;

public interface IConsultationScheduleService {
    public ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request, UserInformationEntity user);
    Page<ConsultationScheduleDTO> getConsultationsByUserAndDepartmentAndTitle(UserInformationEntity user, Integer departmentId, String title, Pageable pageable);

    Page<ConsultationScheduleDTO> getConsultationsByUserAndDepartment(UserInformationEntity user, Integer departmentId, Pageable pageable);

    Page<ConsultationScheduleDTO> searchConsultationsByUserAndTitle(UserInformationEntity user, String title, Pageable pageable);

    Page<ConsultationScheduleDTO> getAllConsultationsByUser(UserInformationEntity user, Pageable pageable);

    Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, Pageable pageable);
    
    void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request, UserInformationEntity consultant);
}
