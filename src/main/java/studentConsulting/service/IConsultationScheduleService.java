package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;

public interface IConsultationScheduleService {
	ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request,
			UserInformationEntity user);

	Page<ConsultationScheduleDTO> getSchedulesByUserWithFilters(UserInformationEntity user, Integer departmentId,
			String title, Pageable pageable);

	Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(UserInformationEntity consultant,
			String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, Pageable pageable);

	void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request,
			UserInformationEntity consultant);
}
