package studentConsulting.service.interfaces.advisor;

import studentConsulting.model.payload.dto.statistic.AdvisorStatisticsDTO;

public interface IAdvisorStatisticsService {
    AdvisorStatisticsDTO getAdvisorStatistics(Integer advisorId);
}

